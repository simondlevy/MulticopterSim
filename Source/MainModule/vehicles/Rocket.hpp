/*
 * General support for rocket vehicles in MulticopterSim
 *
 * Copyright (C) 2020 Simon D. Levy, Daniel Katzav
 *
 * MIT License
 */

#pragma once

#include "../Vehicle.hpp"

class RocketVehicle : public Vehicle {

    public:

        RocketVehicle(Dynamics* dynamics) 
            : Vehicle(dynamics)
        {
        }

    protected:

        virtual void animateActuators(void) override
        {
        }

}; // class RocketVehicle
