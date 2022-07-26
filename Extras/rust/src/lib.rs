#[repr(C)]
pub struct AltHoldPid {

    error_integral: f32,
    in_band:        bool,
    target:         f32,
    throttle:       f32
}

fn _in_band(value : f32, band : f32) -> bool {
    value > -band && value < band
}

fn _constrain_abs(value : f32, limit : f32) -> f32 {
    if value < -limit {-limit} else if value > limit {limit} else {value}
}

fn fabs(value: f32) -> f32 {
    if value < 0.0 {-value} else {value}
}

fn alt_hold(
    throttle: f32,
    altitude: f32,
    _climb_rate: f32,
    _oldpid: AltHoldPid) -> AltHoldPid {

    let _kp = 0.75;
    let _ki = 1.5;
    let altitude_min   = 1.0;
    let _pilot_velz_max = 2.5;
    let stick_deadband = 0.2;
    let _windup_max     = 0.4;

    // Rescale throttle [0,1] => [-1,+1]
    let sthrottle = 2.0 * throttle - 1.0; 

    // Is stick demand in deadband, above a minimum altitude?
    let _in_band = fabs(sthrottle) < stick_deadband && altitude > altitude_min; 

    // Zero throttle will reset error integral
    let _at_zero_throttle = throttle == 0.0;

    AltHoldPid { error_integral: 0.0, in_band: false, target: 0.0, throttle: 0.0 }
}

 #[no_mangle]
pub extern "C" fn rust_alt_hold(
    throttle: f32,
    altitude: f32,
    climb_rate: f32,
    oldpid: *mut AltHoldPid) -> AltHoldPid {

    alt_hold(
        throttle,
        altitude,
        climb_rate,
        AltHoldPid { 
            error_integral: (unsafe { (*oldpid).error_integral}),
            in_band: false,
            target: 0.0,
            throttle: 0.0
        })

}
