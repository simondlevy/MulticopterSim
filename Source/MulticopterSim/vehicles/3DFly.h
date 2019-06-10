/*
* Class declaration for pawn class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "vehicles/QuadXAP.hpp"
#include "GameFramework/Pawn.h"
#include "3DFly.generated.h"

UCLASS(Config=Game)
class MULTICOPTERSIM_API A3DFlyPawn : public APawn {

    private:

        GENERATED_BODY()

        // Physical constants ==============================================

        static constexpr MultirotorDynamics::params_t _params = {

            // Amir's calculations
            5.30216718361085E-05,   // b
            2.23656692806239E-06,   // d
            16.47,                  // m
            0.6,                    // l
            2,                      // Ix
            2,                      // Iy
            3,                      // Iz
            3.08013E-04,            // Jr

            // maxrpm, estimated
            15000                  
        }; 

        static constexpr Vehicle::frame_t _frame = {

            0.0000,  // center X
            0.0000,  // center Y
           -0.0100,  // motor offset
            0.0375,  // width
            0.0375,  // length
            0.0050,  // motor Z
            0.0250   // propeller Z
        };

        // =================================================================

        QuadXAP * _vehicle;
        
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
