/*
 Vehicle class or octo-X frames using ArduPilot motor layout:

        5CCW  1CW
          \   /
      7CW       3CCW
          \   /
            ^     
          /   \
      6CW       8CCW
          /   \
        2CCW  4CW

 Copyright (C) 2019 Simon D. Levy, Daniel Katzav

 MIT License
*/

#pragma once

#include "Vehicle.hpp"

class OctoXAP : public Vehicle {

    private:

        const int8_t directions[4] = {+1, +1, +1, +1};

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
            const int8_t dir[4] = {-1, -1, +1, +1};
            return dir[i];
        }


    public:	

        OctoXAP(const objects_t & objects, const params_t & params)
            : Vehicle(objects, params, 8) 
        {
        }

        static void build(objects_t & objects, const layout_t & layout,
                UStaticMesh * prop1Mesh, UStaticMesh * prop2Mesh, UStaticMesh * prop3Mesh, UStaticMesh * prop4Mesh,
                UStaticMesh * prop5Mesh, UStaticMesh * prop6Mesh, UStaticMesh * prop7Mesh, UStaticMesh * prop8Mesh)
        {
            Vehicle::build(objects);

            Vehicle::addMotor(objects, 0, +1, +1, layout, prop1Mesh);
            Vehicle::addMotor(objects, 1, -1, -1, layout, prop2Mesh);
            Vehicle::addMotor(objects, 2, +1, -1, layout, prop3Mesh);
            Vehicle::addMotor(objects, 3, -1, +1, layout, prop4Mesh);
            Vehicle::addMotor(objects, 4, -1, +1, layout, prop5Mesh);
            Vehicle::addMotor(objects, 5, -1, +1, layout, prop6Mesh);
            Vehicle::addMotor(objects, 6, -1, +1, layout, prop7Mesh);
            Vehicle::addMotor(objects, 7, -1, +1, layout, prop8Mesh);
        }

}; // class OctoXAP
