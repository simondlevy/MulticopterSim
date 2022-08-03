/*
   Hackflight PID controller support

   Copyright (C) 2022 Simon D. Levy

   MIT License
*/

pub mod yaw;
pub mod altitude;

pub mod pids {

    use datatypes::datatypes::Demands;
    use datatypes::datatypes::VehicleState;

    use pids::altitude as altitude_pid;
    use pids::altitude::AltitudePid;

    use pids::yaw as yaw_pid;
    use pids::yaw::YawPid;

    pub struct Controller {

        alt_hold: AltitudePid,
        yaw: YawPid
    }

    pub fn new_controller() -> Controller {

        Controller { alt_hold:altitude_pid::new(), yaw:yaw_pid::new() }
    }

    pub fn run_pids(
        demands: Demands,
        _vehicle_state: VehicleState) -> Demands {

        demands
    }
}

