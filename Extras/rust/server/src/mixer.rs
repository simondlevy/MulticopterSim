pub mod mixer {

    use datatypes::datatypes::Demands;
    use datatypes::datatypes::Motors;

    pub fn run_quadxbf_mixer(demands:Demands) -> Motors {

        Motors {
            m1: demands.throttle - demands.roll + demands.pitch + demands.yaw, // right rear
            m2: demands.throttle - demands.roll - demands.pitch - demands.yaw, // right front
            m3: demands.throttle + demands.roll + demands.pitch - demands.yaw, // left rear
            m4: demands.throttle + demands.roll - demands.pitch + demands.yaw  // left front
        }
    }

} // mod mixer
