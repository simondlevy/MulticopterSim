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

        QuadXAP(APawn * pawn, UStaticMeshComponent * propellerMeshComponents[4], const params_t & params)
            : Vehicle(pawn, propellerMeshComponents, params, 4) 
        {
        }

        static void build(
                APawn * pawn, 
                const layout_t & l,
                UStaticMesh * frameMesh, 
                UStaticMesh * motorMesh,
                UStaticMeshComponent * propellerMeshComponents[4],
                UStaticMesh * prop1Mesh,
                UStaticMesh * prop2Mesh,
                UStaticMesh * prop3Mesh,
                UStaticMesh * prop4Mesh)
        {
            UStaticMeshComponent * frameMeshComponent = Vehicle::build(pawn, frameMesh);

            propellerMeshComponents[0] = addMotor(pawn, frameMeshComponent, "Motor1Mesh", motorMesh, FVector(l.cx+l.wd, l.cy+l.ln+l.mo, l.mz), 
                    "Prop1Mesh", prop1Mesh, FVector(l.cx+l.wd, l.cy+l.ln, l.pz)); 

            propellerMeshComponents[1] = addMotor(pawn, frameMeshComponent, "Motor2Mesh", motorMesh, FVector(l.cx-l.wd, l.cy-l.ln+l.mo, l.mz), 
                    "Prop2Mesh", prop2Mesh, FVector(l.cx-l.wd, l.cy-l.ln, l.pz)); 

            propellerMeshComponents[2] = addMotor(pawn, frameMeshComponent, "Motor3Mesh", motorMesh, FVector(l.cx+l.wd, l.cy-l.ln+l.mo, l.mz),
                    "Prop3Mesh", prop3Mesh, FVector(l.cx+l.wd, l.cy-l.ln, l.pz)); 

            propellerMeshComponents[3] = addMotor(pawn, frameMeshComponent, "Motor4Mesh", motorMesh, FVector(l.cx-l.wd, l.cy+l.ln+l.mo, l.mz),
                    "Prop4Mesh", prop4Mesh, FVector(l.cx-l.wd, l.cy+l.ln, l.pz)); 
        }

}; // class QuadXAP
