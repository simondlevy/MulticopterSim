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
            const int8_t dir[4] = {+1, -1, -1, +1};
            return dir[i];
        }


    public:	

        QuadXAP(
                APawn * pawn, 
                const frame_t * f,
                const params_t * params, 
                UStaticMesh * frameMesh, 
                UStaticMesh * motorMesh,
                UStaticMesh * prop1Mesh,
                UStaticMesh * prop2Mesh,
                UStaticMesh * prop3Mesh,
                UStaticMesh * prop4Mesh)

            : Vehicle(pawn, frameMesh, params, 4)
        {
            addMotor(0, -1, "Motor1Mesh", motorMesh, FVector(f->cx+f->wd, f->cy+f->ln+f->mo, f->mz), 
                    "Prop1Mesh", prop1Mesh, FVector(f->cx+f->wd, f->cy+f->ln, f->pz)); 

            addMotor(1, -1, "Motor2Mesh", motorMesh, FVector(f->cx-f->wd, f->cy-f->ln+f->mo, f->mz), 
                    "Prop2Mesh", prop2Mesh, FVector(f->cx-f->wd, f->cy-f->ln, f->pz)); 

            addMotor(2, +1, "Motor3Mesh", motorMesh, FVector(f->cx+f->wd, f->cy-f->ln+f->mo, f->mz),
                    "Prop3Mesh", prop3Mesh, FVector(f->cx+f->wd, f->cy-f->ln, f->pz)); 

            addMotor(3, +1, "Motor4Mesh", motorMesh, FVector(f->cx-f->wd, f->cy+f->ln+f->mo, f->mz),
                    "Prop4Mesh", prop4Mesh, FVector(f->cx-f->wd, f->cy+f->ln, f->pz)); 
        }

}; // class QuadXAP
