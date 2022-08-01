/*
   Hackflight core algorithm

   Copyright (C) 2022 Simon D. Levy

   MIT License
*/

pub mod hackflight {

    use datatypes::datatypes::AltitudePid;
    use datatypes::datatypes::YawPid;
    use datatypes::datatypes::Demands;
    use datatypes::datatypes::Motors;
    use datatypes::datatypes::VehicleState;

    use mixer::mixer::run_quadxbf_mixer;

    use pids::altitude as altitude_pid;
    use pids::yaw as yaw_pid;

    pub fn run_hackflight(
        demands: Demands,
        vehicle_state: VehicleState, 
        altitude_pid: AltitudePid,
        yaw_pid: YawPid) -> (Motors, AltitudePid, YawPid) {

        let (new_demands, new_altitude_pid) =
            altitude_pid::run(demands, &vehicle_state, altitude_pid.state);

        let (new_new_demands, new_yaw_pid) =
            yaw_pid::run(new_demands, &vehicle_state, yaw_pid.state);

        println!("yaw demand: {}", new_new_demands.yaw);

        let new_motors = run_quadxbf_mixer(new_new_demands.clone());
        
        (new_motors, new_altitude_pid, new_yaw_pid)

    } // run_hackflight

} // mod hackflight
