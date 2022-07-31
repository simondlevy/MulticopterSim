/*
   Mixers

   Copyright (C) 2022 Simon D. Levy

   MIT License
*/

pub mod mixer {

    use datatypes::datatypes::Demands;
    use datatypes::datatypes::Motors;

    pub fn run_quadxbf_mixer(demands:Demands) -> Motors {

        Motors {

            // right rear
            m1: demands.throttle - demands.roll + demands.pitch + demands.yaw, 

            // right front
            m2: demands.throttle - demands.roll - demands.pitch - demands.yaw, 

            // left rear
            m3: demands.throttle + demands.roll + demands.pitch - demands.yaw, 

            // left front
            m4: demands.throttle + demands.roll - demands.pitch + demands.yaw  
        }
    }

} // mod mixer
