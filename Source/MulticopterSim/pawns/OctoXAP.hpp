/*
 Vehicle class or octo-X frames using ArduPilot motor layout:

        5CCW  1CW
               
    7CW          3CCW
               
            ^     
               
    6CW          8CCW
               
        2CCW  4CW

 Copyright (C) 2019 Simon D. Levy, Daniel Katzav

 MIT License
*/

#pragma once

#include "Vehicle.hpp"

class OctoXAP : public Vehicle {

    private:

        const int8_t directions[8] = {+1, +1, -1, -1, -1, -1, +1, +1};

    protected:

        // MultirotorDynamics method overrides
        // XXX For now we use quadXAP dynamics


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
            const int8_t dir[8] = {+1, +1, -1, -1, -1, -1, +1, +1};
            return dir[i];
        }


    public:	

        OctoXAP(const objects_t & objects, const params_t & params)
            : Vehicle(objects, params, 8) 
        {
        }

}; // class OctoXAP
