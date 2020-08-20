/*
 * General support for multirotor vehicles in MulticopterSim
 *
 * Copyright (C) 2020 Simon D. Levy, Daniel Katzav
 *
 * MIT License
 */

#pragma once

#include "../Vehicle.hpp"

class MultirotorVehicle : public Vehicle {

    public:

        MultirotorVehicle(Dynamics* dynamics) 
            : Vehicle(dynamics)
        {
        }


}; // class MultirotorVehicle
