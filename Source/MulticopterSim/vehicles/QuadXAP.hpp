/*
* Vehicle class for quad-X frames using ArduPilot motor layout:
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "Vehicle.hpp"
#include "dynamics/QuadXAPDynamics.hpp"

class QuadXAP : public Vehicle {

    private:

    protected:

    public:	

        QuadXAP(APawn * pawn, UStaticMesh * frameMesh, const MultirotorDynamics::params_t * params)
            : Vehicle(pawn, frameMesh, new QuadXAPDynamics(params), 4)
        {
        }

}; // class QuadXAP
