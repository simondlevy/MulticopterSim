/*
* Class declaration for pawn class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "pawns/OctoXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Goshawk.generated.h"

UCLASS(Config=Game)
class MULTICOPTERSIM_API AGoshawkPawn : public APawn {

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

        // The Vehicle object will handle most of the work for our vehicle pawn
        Vehicle * _vehicle = NULL;

        // Helper for adding prop meshes
        static void addProp(Vehicle::objects_t & objects, uint8_t index, float x, float y, UStaticMesh * mesh)
        {
            Vehicle::addProp(objects, index, x, y, -0.04, mesh);
        }

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override;

    public:	

        AGoshawkPawn();

        ~AGoshawkPawn();

}; // AGoshawkPawn
