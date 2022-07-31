pub mod mixer {

    use datatypes::datatypes::Demands;
    use datatypes::datatypes::Motors;

    pub fn run_mixer(demands:Demands) -> Motors {

        Motors {
            m1: demands.throttle,
            m2: demands.throttle,
            m3: demands.throttle,
            m4: demands.throttle
        }
    }

} // mod mixer
