use datatypes::datatypes::Hackflight;
use datatypes::datatypes::AltHoldPid;

use alt_hold::alt_hold::run_alt_hold;

pub mod datatypes;
pub mod alt_hold;

#[no_mangle]
pub extern "C" fn rust_run_hackflight(hackflight: *mut Hackflight) -> AltHoldPid {

    let demands = unsafe { &(*hackflight).demands };
    let vehicle_state = unsafe { &(*hackflight).vehicle_state };
    let alt_hold_pid = unsafe { &(*hackflight).alt_hold_pid };

    run_alt_hold(
        demands.throttle,
        -vehicle_state.z,  // NED => ENU
        -vehicle_state.dz, // NED => ENU
        AltHoldPid { 
            error_integral: alt_hold_pid.error_integral,
            in_band: alt_hold_pid.in_band,
            target: alt_hold_pid.target,
            throttle: alt_hold_pid.throttle
        })
}
