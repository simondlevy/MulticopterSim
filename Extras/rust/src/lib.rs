use datatypes::datatypes::Hackflight;
use datatypes::datatypes::AltHoldPid;

use alt_hold::alt_hold::run_alt_hold;

pub mod datatypes;
pub mod alt_hold;

#[no_mangle]
pub extern "C" fn rust_run_hackflight(
    hackflight: *mut Hackflight,
    oldpid: *mut AltHoldPid) -> AltHoldPid {

    let demands = unsafe { &(*hackflight).demands };
    let vehicle_state = unsafe { &(*hackflight).vehicle_state };

    run_alt_hold(
        demands.throttle,
        -vehicle_state.z,  // NED => ENU
        -vehicle_state.dz, // NED => ENU
        AltHoldPid { 
            error_integral: (unsafe { (*oldpid).error_integral}),
            in_band: (unsafe { (*oldpid).in_band}),
            target: (unsafe { (*oldpid).target}),
            throttle: (unsafe { (*oldpid).throttle})
        })

}
