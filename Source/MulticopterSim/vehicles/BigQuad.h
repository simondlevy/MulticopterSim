/*
* Class declaration for pawn class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "vehicles/QuadXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "BigQuad.generated.h"

UCLASS(Config=Game)
class MULTICOPTERSIM_API ABigQuadPawn : public APawn {

    private:

        GENERATED_BODY()

        static constexpr MultirotorDynamics::params_t _params = {

            // Dynamics: Amir's calculations
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

        // Frame layout, for placing motor and propellers
        static constexpr Vehicle::frame_t _frame = {

            -0.59, // cx
            +0.13, // cy
             0.01, // mo
             0.35, // wd
             0.35, // ln
             0.45, // mz
             0.45  // pz
        };

        // The Vehicle object will handle most of the work for our vehicle pawn
        Vehicle * _vehicle = NULL;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override;

    public:	

        ABigQuadPawn();

        ~ABigQuadPawn();

}; // ABigQuadPawn
