/*
   Hackflight core algorithm

   Copyright (C) 2022 Simon D. Levy

   MIT License
*/

pub mod hackflight {

    use datatypes::datatypes::AltHoldPid;
    use datatypes::datatypes::AnglePid;
    use datatypes::datatypes::Demands;
    use datatypes::datatypes::Motors;
    use datatypes::datatypes::VehicleState;

    use alt_hold::alt_hold::run_alt_hold;
    use angle_pid::angle_pid::run_angle_pid;
    use mixer::mixer::run_quadxbf_mixer;

    pub fn run_hackflight(
        demands: Demands,
        vehicle_state: VehicleState, 
        alt_hold_pid: AltHoldPid,
        angle_pid: AnglePid) -> (Motors, AltHoldPid, AnglePid) {

        let (new_demands, new_alt_hold_pid) =
            run_alt_hold(demands, &vehicle_state, alt_hold_pid.state);

        let (new_new_demands, new_angle_pid) =
            run_angle_pid(new_demands, &vehicle_state, angle_pid.state);

        println!("yaw demand: {}", new_new_demands.yaw);

        let new_motors = run_quadxbf_mixer(new_new_demands.clone());

        (new_motors, new_alt_hold_pid, new_angle_pid)

    } // run_hackflight

} // mod hackflight
