use datatypes::datatypes::Demands;
use datatypes::datatypes::VehicleState;

pub mod datatypes;

fn main() {

    let _vehicle_state = VehicleState {
        z:0.0,
        dz:0.0
    };

    let _demands = Demands {

        throttle:0.0,
        roll:0.0,
        pitch:0.0,
        yaw:0.0
    };

    loop {

    }
}
