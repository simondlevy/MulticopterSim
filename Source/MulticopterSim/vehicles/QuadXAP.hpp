/*
* Vehicle class or quad-X frames using ArduPilot motor layout:
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "Vehicle.hpp"
#include "dynamics/QuadXAPDynamics.hpp"

class QuadXAP : public Vehicle {

    private:

    protected:

    public:	

        QuadXAP(
                APawn * pawn, 
                const frame_t * f,
                const MultirotorDynamics::params_t * params, 
                UStaticMesh * frameMesh, 
                UStaticMesh * motorMesh,
                UStaticMesh * prop1Mesh,
                UStaticMesh * prop2Mesh,
                UStaticMesh * prop3Mesh,
                UStaticMesh * prop4Mesh)

            : Vehicle(pawn, frameMesh, new QuadXAPDynamics(params), 4)
        {
            addMotor(0, -1, TEXT("Motor1Mesh"), motorMesh, FVector(f->cx+f->wd, f->cy+f->ln+f->mo, f->mz), 
                    TEXT("Prop1Mesh"), prop1Mesh, FVector(f->cx+f->wd, f->cy+f->ln, f->pz)); 

            addMotor(1, -1, TEXT("Motor2Mesh"), motorMesh, FVector(f->cx-f->wd, f->cy-f->ln+f->mo, f->mz), 
                    TEXT("Prop2Mesh"), prop2Mesh, FVector(f->cx-f->wd, f->cy-f->ln, f->pz)); 

            addMotor(2, +1, TEXT("Motor3Mesh"), motorMesh, FVector(f->cx+f->wd, f->cy-f->ln+f->mo, f->mz),
                    TEXT("Prop3Mesh"), prop3Mesh, FVector(f->cx+f->wd, f->cy-f->ln, f->pz)); 

            addMotor(3, +1, TEXT("Motor4Mesh"), motorMesh, FVector(f->cx-f->wd, f->cy+f->ln+f->mo, f->mz),
                    TEXT("Prop4Mesh"), prop4Mesh, FVector(f->cx-f->wd, f->cy+f->ln, f->pz)); 
        }

}; // class QuadXAP
