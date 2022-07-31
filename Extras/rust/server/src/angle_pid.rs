/*
   Angle PID controller

   Copyright (C) 2022 Simon D. Levy

   MIT License
*/

pub mod angle_pid {

    use datatypes::datatypes::AnglePid;
    use datatypes::datatypes::Demands;
    use datatypes::datatypes::VehicleState;

    #[repr(C)]
    #[derive(Clone)]
    pub struct AnglePidState {
    }

    pub fn run_angle_pid(
        demands:Demands,
        _vstate:&VehicleState,
        _pstate: AnglePidState) -> (Demands, AnglePid) {

        let new_demands = Demands {
            throttle:demands.throttle,
            roll:demands.roll,
            pitch:demands.pitch,
            yaw:demands.yaw
        };

        let new_angle_pid = make_angle_pid();

        (new_demands, new_angle_pid)
    }

    fn make_angle_pid() -> AnglePid {
        AnglePid {
            state: AnglePidState {
            }
        }
    }

    pub fn new_angle_pid() -> AnglePid {
        make_angle_pid()
    }

} // mod angle_pid
