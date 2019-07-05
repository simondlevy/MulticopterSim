/*
* Dynamics class for quad-X frames using ArduPilot motor layout:
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

#include "dynamics/MultirotorDynamics.hpp"

class QuadXAPDynamics : public MultirotorDynamics {

    public:	

		QuadXAPDynamics(const params_t & params) : MultirotorDynamics(params, 4)
        {
        }

    protected:

        // MultirotorDynamics method overrides

        // roll right
        virtual double u2(double * o) override
        {
            return (o[1] + o[2]) - (o[0] + o[3]);
        }

        // pitch forward
        virtual double u3(double * o) override
        {
            return (o[1] + o[3]) - (o[0] + o[2]);
        }

        // yaw cw
        virtual double u4(double * o) override
        {
            return (o[0] + o[1]) - (o[2] + o[3]);
        }

        // motor direction for animation
        virtual int8_t motorDirection(uint8_t i) override
        {
            const int8_t dir[4] = {-1, -1, +1, +1};
            return dir[i];
        }

}; // class QuadXAP
