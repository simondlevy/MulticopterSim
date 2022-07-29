use datatypes::datatypes::AltHoldPid;
use datatypes::datatypes::Demands;
use datatypes::datatypes::VehicleState;

pub mod datatypes;

fn main() {

    let _vehicle_state = VehicleState {
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

    let _demands = Demands {

        throttle:0.0,
        roll:0.0,
        pitch:0.0,
        yaw:0.0
    };

    let _alt_hold_pid = AltHoldPid {
        error_integral: 0.0,
        in_band: false,
        target: 0.0
    };

    loop {

    }
}
