use datatypes::datatypes::AltHoldPid;
use datatypes::datatypes::Demands;
use datatypes::datatypes::Hackflight;
use datatypes::datatypes::Motors;
use datatypes::datatypes::VehicleState;

use hackflight::hackflight::run_hackflight;

pub mod datatypes;
pub mod hackflight;
pub mod alt_hold;

#[no_mangle]
pub extern "C" fn rust_run_hackflight(c_hackflight: *mut Hackflight) -> Hackflight {

    let c_demands = unsafe { &(*c_hackflight).demands };
    let c_vehicle_state = unsafe { &(*c_hackflight).vehicle_state };
    let c_alt_hold_pid = unsafe { &(*c_hackflight).alt_hold_pid };

    let vehicle_state = VehicleState {
        z: c_vehicle_state.z,
        dz: c_vehicle_state.dz
    };

    let demands = Demands { 
        throttle: c_demands.throttle,
        roll: c_demands.roll,
        pitch: c_demands.pitch,
        yaw: c_demands.yaw
    };

    let alt_hold_pid = AltHoldPid {
        error_integral: c_alt_hold_pid.error_integral,
        in_band: c_alt_hold_pid.in_band,
        target: c_alt_hold_pid.target
    };

    let motors = Motors {
        m1: 0.0,
        m2: 0.0,
        m3: 0.0,
        m4: 0.0
    };

    let hackflight = Hackflight { 
        demands: demands,
        vehicle_state: vehicle_state,
        alt_hold_pid: alt_hold_pid,
        motors: motors
    };

    run_hackflight(hackflight)
}
