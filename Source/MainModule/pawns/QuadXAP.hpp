/*
* Vehicle class or quad-X frames using ArduPilot motor layout:
*
*    3cw   1ccw
*       \ /
*        ^
*       / \
*    2ccw  4cw
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "QuadX.hpp"
#include "dynamics/QuadXAP.hpp"

class QuadXAP : public QuadX {

    public:	

        QuadXAP(const objects_t & objects, const MultirotorDynamics::params_t & params)
            : QuadX(objects, new QuadXAPDynamics(params)) 
        {
        }

}; // class QuadXAP
