use datatypes::datatypes::Hackflight;
use datatypes::datatypes::Demands;
use datatypes::datatypes::VehicleState;
use datatypes::datatypes::AltHoldPid;
use datatypes::datatypes::Motors;

use alt_hold::alt_hold::run_alt_hold;

pub mod datatypes;
pub mod alt_hold;

#[no_mangle]
pub extern "C" fn rust_run_hackflight(hackflight: *mut Hackflight) -> Hackflight {

    let demands = unsafe { &(*hackflight).demands };
    let vehicle_state = unsafe { &(*hackflight).vehicle_state };
    let alt_hold_pid = unsafe { &(*hackflight).alt_hold_pid };

    let (new_throttle, new_alt_hold_pid) = run_alt_hold(
        demands.throttle,
        -vehicle_state.z,  // NED => ENU
        -vehicle_state.dz, // NED => ENU
        AltHoldPid { 
            error_integral: alt_hold_pid.error_integral,
            in_band: alt_hold_pid.in_band,
            target: alt_hold_pid.target
        });

    let new_vehicle_state = VehicleState {
        x: 0.0,
        dx: 0.0,
        y: 0.0,
        dy: 0.0,
        z: 0.0,
        dz: 0.0,
        phi: 0.0,
        dphi: 0.0,
        theta: 0.0,
        dtheta: 0.0,
        psi: 0.0,
        dpsi: 0.0
    };

    let new_demands = Demands {
        throttle: new_throttle,
        roll: 0.0,
        pitch: 0.0,
        yaw: 0.0
    };

    let new_motors = Motors {
        m1: new_demands.throttle,
        m2: new_demands.throttle,
        m3: new_demands.throttle,
        m4: new_demands.throttle
    };

    Hackflight { 
        demands: new_demands,
        vehicle_state: new_vehicle_state,
        alt_hold_pid: new_alt_hold_pid,
        motors: new_motors
    }
}
