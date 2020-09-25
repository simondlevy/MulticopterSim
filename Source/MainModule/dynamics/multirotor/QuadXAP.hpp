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

#include "../../Dynamics.hpp"

class QuadXAPDynamics : public Dynamics {

    public:	

        QuadXAPDynamics(
                const double b, 
                const double d, 
                const double m, 
                const double Ix, 
                const double Iy, 
                const double Iz, 
                const double Jr, 
                const double l,
                uint16_t maxrpm) 
            : Dynamics(4, b, d, m, Ix, Iy, Iz, Jr, l, maxrpm)
        {
        }

    protected:

        // Dynamics method overrides

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

        // rotor direction for animation
        virtual int8_t rotorDirection(uint8_t i) override
        {
            const int8_t dir[4] = {-1, -1, +1, +1};
            return dir[i];
        }

}; // class QuadXAP
