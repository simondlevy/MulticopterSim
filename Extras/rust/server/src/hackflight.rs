pub mod hackflight {

    use datatypes::datatypes::AltHoldPid;
    use datatypes::datatypes::Demands;
    use datatypes::datatypes::Motors;
    use datatypes::datatypes::VehicleState;

    use alt_hold::alt_hold::run_alt_hold;

    pub fn run_mixer(demands:Demands) -> Motors {

        Motors {
            m1: demands.throttle,
            m2: demands.throttle,
            m3: demands.throttle,
            m4: demands.throttle
        }
    }


    pub fn run_hackflight(
        demands: Demands,
        vehicle_state: VehicleState, 
        alt_hold_pid: AltHoldPid) -> (AltHoldPid, Motors) {

        let (new_throttle, new_alt_hold_pid) = run_alt_hold(
            demands.throttle,
            -vehicle_state.z,  // NED => ENU
            -vehicle_state.dz, // NED => ENU
            alt_hold_pid);

        let new_demands = Demands {
            throttle: new_throttle,
            roll: 0.0,
            pitch: 0.0,
            yaw: 0.0
        };

        let new_motors = run_mixer(new_demands.clone());

        (new_alt_hold_pid, new_motors)

    } // run_hackflight

} // mod hackflight