//use alt_hold::alt_hold::AltHoldPid;
use datatypes::datatypes::AltHoldPid;
use alt_hold::alt_hold::run_alt_hold;

pub mod datatypes;
pub mod alt_hold;

#[no_mangle]
pub extern "C" fn rust_run_alt_hold(
    throttle: f32,
    altitude: f32,
    climb_rate: f32,
    oldpid: *mut AltHoldPid) -> AltHoldPid {

    run_alt_hold(
        throttle,
        altitude,
        climb_rate,
        AltHoldPid { 
            error_integral: (unsafe { (*oldpid).error_integral}),
            in_band: (unsafe { (*oldpid).in_band}),
            target: (unsafe { (*oldpid).target}),
            throttle: (unsafe { (*oldpid).throttle})
        })

}
