/*
   Angle PID controller

   Copyright (C) 2022 Simon D. Levy

   MIT License
*/

pub mod angle_pid {

    use datatypes::datatypes::AnglePid;
    use datatypes::datatypes::Demands;
    use datatypes::datatypes::VehicleState;

    use utils::utils::fabs;
    use utils::utils::constrain_abs;
    use utils::utils::deg2rad;

    #[repr(C)]
    #[derive(Clone)]
    pub struct AnglePidState {
        error_integral: f32
    }

    pub fn run_angle_pid(
        demands:Demands,
        vstate:&VehicleState,
        pstate: AnglePidState) -> (Demands, AnglePid) {

        const KP: f32 = 1.0625;
        const KI: f32 = 0.001875;

        const WINDUP_MAX: f32 = 6.0;
        const RATE_MAX_DPS: f32 = 45.0;

        // Compute error as difference between yaw demand and angular velocity
        let error = demands.yaw - vstate.dpsi;

        // Reset integral on quick angular velocity change
        let error_integral =
            if fabs(error) > deg2rad(RATE_MAX_DPS) {0.0} else {pstate.error_integral};

        // Constrain integral to avoid windup
        let _error_integral_bounded = constrain_abs(error_integral + error, WINDUP_MAX);

        // Adjust yaw demand based on error
        //demands[DEMANDS_YAW] = _Kp * error + _Ki * _errorI;

        let new_demands = Demands {
            throttle:demands.throttle,
            roll:demands.roll,
            pitch:demands.pitch,
            yaw:demands.yaw
        };

        let new_angle_pid = make_angle_pid(error_integral);

        (new_demands, new_angle_pid)
    }

    fn make_angle_pid(error_integral:f32) -> AnglePid {
        AnglePid {
            state: AnglePidState {
                error_integral:error_integral
            }
        }
    }

    pub fn new_angle_pid() -> AnglePid {
        make_angle_pid(0.0)
    }

} // mod angle_pid
