/*
* Dynamics class for simulated dragonfly:
*
*      3   1
*       \ /
*        ^
*       / \
*      2   4
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../Dynamics.hpp"

class DragonflyDynamics : public Dynamics {

    public:	

        DragonflyDynamics(Dynamics::vehicle_params_t & vparams) 
            : Dynamics(4, vparams)
        {
        }

    protected:

        // Dynamics method overrides

        virtual void computeTorques(double * motorvals, double & u2, double & u3, double & u4) override
        {
            // motor values are needed only for thrust vectoring
            (void)motorvals;

            // shorthand
            double * o = _omegas2;

            // roll right
            u2 = (o[1] + o[2]) - (o[0] + o[3]);

            // pitch forward
            u3 = (o[1] + o[3]) - (o[0] + o[2]);

            // yaw clockwise
            u4 = (o[0] + o[1]) - (o[2] + o[3]);
        }

        // rotor direction for animation
        virtual int8_t rotorDirection(uint8_t i) override
        {
            const int8_t dir[4] = {+1, -1, -1, +1};
            return dir[i];
        }

}; // class DragonflyDynamics
