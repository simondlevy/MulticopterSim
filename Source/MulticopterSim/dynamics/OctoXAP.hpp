/*
* Dynamics class for quad-X frames using ArduPilot motor layout:
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

#include "dynamics/MultirotorDynamics.hpp"

class OctoXAPDynamics : public MultirotorDynamics {

    public:	

		OctoXAPDynamics(const params_t & params) : MultirotorDynamics(params, 8)
        {
        }

    protected:

        // MultirotorDynamics method overrides

        // roll right
        virtual double u2(double * o) override
        {
            //       [2      5      6      7]  -   [1      3      4      8]
            return 0;//(o[1] + o[4] + o[5] + o[6]) - (o[0] + o[2] + o[3] + o[7]);
        }

        // pitch forward
        virtual double u3(double * o) override
        {
            //       [2      4      6      8]  -   [1      3      5      7]
            return 0;//(o[1] + o[3] + o[5] + o[7]) - (o[0] + o[2] + o[4] + o[6]);
        }

        // yaw cw
        virtual double u4(double * o) override
        {
            //       [1      2      7      8]  -   [3      4      5      6]
            return (o[0] + o[1] + o[6] + o[7]) - (o[2] + o[3] + o[4] + o[5]);
        }

        // motor direction for animation
        virtual int8_t motorDirection(uint8_t i) override
        {
            //                      1   2   3   4   5   6   7   8                                 
            const int8_t dir[8] = {+1, +1, -1, -1, -1, -1, +1, +1};
            return dir[i];
        }

}; // class OctoXAP
