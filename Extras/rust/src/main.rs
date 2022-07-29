use datatypes::datatypes::AltHoldPid;
use datatypes::datatypes::Demands;
use datatypes::datatypes::VehicleState;

use hackflight::hackflight::run_hackflight2;

pub mod alt_hold;
pub mod datatypes;
pub mod hackflight;

fn main() {

    let mut alt_hold_pid = AltHoldPid {
        error_integral: 0.0,
        in_band: false,
        target: 0.0
    };

    loop {

        let demands = Demands {

            throttle:0.0,
            roll:0.0,
            pitch:0.0,
            yaw:0.0
        };

        let vehicle_state = VehicleState {
            x:0.0,
            dx:0.0,
            y:0.0,
            dy:0.0,
            z:0.0,
            dz:0.0,
            phi:0.0,
            dphi:0.0,
            theta:0.0,
            dtheta:0.0,
            psi:0.0,
            dpsi:0.0
        };

        let (new_alt_hold_pid, _motors) =
            run_hackflight2(demands, vehicle_state, alt_hold_pid.clone());

        alt_hold_pid.error_integral = new_alt_hold_pid.error_integral;
    }
}
