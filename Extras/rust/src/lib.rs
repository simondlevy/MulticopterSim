#[repr(C)]

#[repr(C)]
pub struct AltHold {

    error_integral: f32,
    in_band:        bool,
    target:         f32,
    throttle:       f32
}

fn in_band(value : f32, band : f32) -> bool {
    value > -band && value < band
}

fn constrain_abs(value : f32, limit : f32) -> f32 {
    if value < -limit {-limit} else if value > limit {limit} else {value}
}

 #[no_mangle]
pub extern "C" fn rust_alt_hold() {
}
