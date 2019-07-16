/*
* Class declaration for pawn class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy, Daniel Katzav
*
* MIT License
*/

#pragma once

#include "pawns/QuadXAP.hpp"
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

        QuadXAP::layout_t _layout = {

            0.12,  // width
            0.12,  // length
            0.00,  // center X
            0.00,  // center Y
            0.15,  // propeller Z
            0.00,  // motor offset
            0.00   // motor Z
        };

        QuadXAP * _vehicle;
        
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
