/*
* Class declaration for pawn class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "dynamics/QuadXAP.hpp"
#include "Vehicle.hpp"
#include "GameFramework/Pawn.h"
#include "3DFly.generated.h"

UCLASS(Config=Game)
class MAINMODULE_API A3DFlyPawn : public APawn {

    private:

        GENERATED_BODY()

        MultirotorDynamics::params_t _params = {

            // Estimated

            5.30E-07,               // b
            2.24E-06,               // d
            0.110,                  // m
            0.050,                  // l
            0.02,                   // Ix
            0.02,                   // Iy
            0.03,                   // Iz
            3.08E-04,               // Jr
            
            10000,                  // maxrpm

            0                       // XXX motor acceleration }; 
         };

        // Container for frame layout constants
        typedef struct {

            float wd;   // width
            float ln;   // length
            float cx;   // center X
            float cy;   // center Y
            float pz;   // propeller Z
            float mo;   // motor offset
            float mz;   // motor Z

        } layout_t;

        Vehicle * _vehicle;
        
    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        A3DFlyPawn();

        ~A3DFlyPawn();

}; // A3DFlyPawn
