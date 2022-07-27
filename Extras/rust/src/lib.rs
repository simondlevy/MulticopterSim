use datatypes::datatypes::AltHoldPid;
use datatypes::datatypes::Demands;
use datatypes::datatypes::VehicleState;
use alt_hold::alt_hold::run_alt_hold;

pub mod datatypes;
pub mod alt_hold;

#[no_mangle]
pub extern "C" fn rust_run_alt_hold(
    demands: *mut Demands,
    vehicle_state: *mut VehicleState,
    oldpid: *mut AltHoldPid) -> AltHoldPid {

    run_alt_hold(
        unsafe { (*demands).throttle},
        -(unsafe {(*vehicle_state).z}),  // NED => ENU
        -(unsafe {(*vehicle_state).dz}), // NED => ENU
        AltHoldPid { 
            error_integral: (unsafe { (*oldpid).error_integral}),
            in_band: (unsafe { (*oldpid).in_band}),
            target: (unsafe { (*oldpid).target}),
            throttle: (unsafe { (*oldpid).throttle})
        })

}
