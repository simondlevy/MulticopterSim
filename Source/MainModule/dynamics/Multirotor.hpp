/*
* Dynamics class for multirotors
*
* Copyright (C) 2020 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../Dynamics.hpp"

class MultirotorDynamics : public Dynamics {

    public:

        MultirotorDynamics(Parameters * params, const uint8_t motorCount) 
            : Dynamics(params, motorCount)
        {
        }


}; // class MultirotorDynamics
