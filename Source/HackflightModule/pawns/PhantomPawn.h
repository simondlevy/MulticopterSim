/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../SimQuadMixer.hpp"
#include "../../MainModule/vehicles/Phantom.hpp"
#include "../FlightManager.hpp"

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "PhantomPawn.generated.h"

UCLASS(Config=Game)
class APhantomPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper classes
        Phantom _phantom;
        SimQuadMixer _mixer;

        FHackflightFlightManager * _flightManager = NULL;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        APhantomPawn();

}; // APhantomPawn
