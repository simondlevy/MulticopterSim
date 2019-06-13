/*
* Vehicle class or quad-X frames using ArduPilot motor layout:
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "Vehicle.hpp"

class QuadXAP : public Vehicle {

    private:

        const int8_t directions[4] = {+1, +1, +1, +1};

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


    public:	

        QuadXAP(const objects_t & objects, const params_t & params)
            : Vehicle(objects, params, 4) 
        {
        }

        static void build(objects_t & objects, const layout_t & l,
                UStaticMesh * prop1Mesh, UStaticMesh * prop2Mesh, UStaticMesh * prop3Mesh, UStaticMesh * prop4Mesh)
        {
            Vehicle::build(objects);

            addMotor(objects, 0, FVector(l.cx+l.wd, l.cy+l.ln+l.mo, l.mz), prop1Mesh, FVector(l.cx+l.wd, l.cy+l.ln, l.pz)); 
            addMotor(objects, 1, FVector(l.cx-l.wd, l.cy-l.ln+l.mo, l.mz), prop2Mesh, FVector(l.cx-l.wd, l.cy-l.ln, l.pz)); 
            addMotor(objects, 2, FVector(l.cx+l.wd, l.cy-l.ln+l.mo, l.mz), prop3Mesh, FVector(l.cx+l.wd, l.cy-l.ln, l.pz)); 
            addMotor(objects, 3, FVector(l.cx-l.wd, l.cy+l.ln+l.mo, l.mz), prop4Mesh, FVector(l.cx-l.wd, l.cy+l.ln, l.pz)); 
        }

}; // class QuadXAP
