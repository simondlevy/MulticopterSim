pub mod alt_hold {

    use datatypes::datatypes::AltHoldPid;

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
        oldpid: AltHoldPid) -> (f32, AltHoldPid) {

        const KP: f32 = 0.75;
        const KI: f32 = 1.5;
        const ALTITUDE_MIN: f32   = 1.0;
        const PILOT_VELZ_MAX: f32 = 2.5;
        const STICK_DEADBAND: f32 = 0.2;
        const WINDUP_MAX: f32     = 0.4;

        // Rescale throttle [0,1] => [-1,+1]
        let sthrottle = throttle; // 2.0 * throttle - 1.0; 

        // Is stick demand in deadband, above a minimum altitude?
        let in_band = fabs(sthrottle) < STICK_DEADBAND && altitude > ALTITUDE_MIN; 

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
            if in_band {new_target - altitude} else {PILOT_VELZ_MAX * sthrottle};

        // Compute error as scaled target minus actual
        let error = target_velocity - climb_rate;

        // Compute I term, avoiding windup
        let new_error_integral = constrain_abs(oldpid.error_integral + error, WINDUP_MAX);

        // Run PI controller
        let correction = error * KP + new_error_integral * KI;

        // Add correction to throttle, constraining output to [0,1]
        let new_throttle = constrain(throttle+correction, 0.0, 1.0);

        // Capture new state of PID controller
        let new_alt_hold_pid = AltHoldPid {
            error_integral: new_error_integral,
            in_band: in_band,
            target: new_target
        };

        (new_throttle, new_alt_hold_pid)
    }

}
