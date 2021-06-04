/*
* Dynamics class for octo-X frames using ArduPilot motor layout:
*
*        5CCW   1CW
*                  
*    7CW           3CCW
*                   
*             ^      
*                   
*    6CW           8CCW
*                   
*        2CCW   4CW
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../FixedPitch.hpp"

class OctoXAPDynamics : public FixedPitchDynamics {

    public:	

        OctoXAPDynamics(Dynamics::vehicle_params_t &vparams, FixedPitchDynamics::fixed_pitch_params_t &fparams)
            : FixedPitchDynamics(8, vparams, fparams)
        {
        }

    protected:

        // FixedPitchDynamics method overrides

        virtual double u2(void) override
        {
            // shorthand
            double * o = _omegas2;

            // roll right
            //        [2         5         6         7]     - [  1         3         4         8]
            return = (C1*o[1] + C1*o[4] + C2*o[5] + C2*o[6]) - (C1*o[0] + C2*o[2] + C1*o[3] + C2*o[7]);
        }

        virtual double u3 (void) override
        {
            // shorthand
            double * o = _omegas2;

            //       [ 2        4         6         8]   -   [  1         3         5         7]
            return (C2*o[1] + C2*o[3] + C1*o[5] + C1*o[7]) - (C2*o[0] + C1*o[2] + C2*o[4] + C1*o[6]);
        }

        virtual double u4(void) override
        {
            // shorthand
            double * o = _omegas2;

            //       [3      4      5      6]  -   [1      2      7      8]
            return (o[2] + o[3] + o[4] + o[5]) - (o[0] + o[1] + o[6] + o[7]);
        }

        // rotor direction for animation
        virtual int8_t rotorDirection(uint8_t i) override
        {
            //                      1   2   3   4   5   6   7   8                                 
            const int8_t dir[8] = {+1, +1, -1, -1, -1, -1, +1, +1};
            return dir[i];
        }

    private:

        static constexpr double C1 = 0.382680;
        static constexpr double C2 = 0.923879;

}; // class OctoXAP
