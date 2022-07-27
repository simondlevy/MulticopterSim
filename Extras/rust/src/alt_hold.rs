pub mod alt_hold {

    #[repr(C)]
    pub struct AltHoldPid {

        pub error_integral: f32,
        pub in_band:        bool,
        pub target:         f32,
        pub throttle:       f32
    }

    fn constrain(value: f32, lo: f32, hi: f32) -> f32 {
        if value  < lo {lo} else if value > hi {hi} else {value}
    }

    fn constrain_abs(value : f32, limit : f32) -> f32 {
        constrain(value, -limit, limit)
    }

    fn fabs(value: f32) -> f32 {
        if value < 0.0 {-value} else {value}
    }

    pub fn run_alt_hold(
        throttle: f32,
        altitude: f32,
        climb_rate: f32,
        oldpid: AltHoldPid) -> AltHoldPid {

        let kp = 0.75;
        let ki = 1.5;
        let altitude_min   = 1.0;
        let pilot_velz_max = 2.5;
        let stick_deadband = 0.2;
        let windup_max     = 0.4;

        // Rescale throttle [0,1] => [-1,+1]
        let sthrottle = 2.0 * throttle - 1.0; 

        // Is stick demand in deadband, above a minimum altitude?
        let in_band = fabs(sthrottle) < stick_deadband && altitude > altitude_min; 

        // Zero throttle will reset error integral
        let at_zero_throttle = throttle == 0.0;

        // Reset altitude target at zero throttle
        let altitude_target = if at_zero_throttle {0.0} else {oldpid.target};

        // If stick just moved into deadband, set new target altitude; otherwise,
        // keep previous
        let new_target = if in_band && !oldpid.in_band {altitude} else {altitude_target};

        // Target velocity is a setpoint inside deadband, scaled
        // constant outside
        let target_velocity =
            if in_band {new_target - altitude} else {pilot_velz_max * sthrottle};

        // Compute error as scaled target minus actual
        let error = target_velocity - climb_rate;

        // Compute I term, avoiding windup
        let new_error_integral = constrain_abs(oldpid.error_integral + error, windup_max);

        // Run PI controller
        let correction = error * kp + new_error_integral * ki;

        // Add correction to throttle, constraining output to [0,1]
        let new_throttle = constrain(throttle+correction, 0.0, 1.0);

        AltHoldPid {
            error_integral: new_error_integral,
            in_band: in_band,
            target: new_target,
            throttle: new_throttle
        }
    }

}
