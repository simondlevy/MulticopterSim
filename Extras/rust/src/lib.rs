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

 #[no_mangle]
pub extern "C" fn rust_alt_hold(
    _throttle: f32,
    _altitude: f32,
    _climb_rate: f32,
    _oldpid: *mut AltHoldPid) -> AltHoldPid {

    AltHoldPid { error_integral: 0.0, in_band: false, target: 0.0, throttle: 0.0 }
}
