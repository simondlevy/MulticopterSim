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
#include "Phantom.generated.h"

UCLASS(Config=Game)
class MAINMODULE_API APhantomPawn : public APawn {

    private:

        GENERATED_BODY()

        MultirotorDynamics::params_t _params = {

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

        Vehicle * _vehicle;
        
    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        APhantomPawn();

        ~APhantomPawn();

}; // APhantomPawn
