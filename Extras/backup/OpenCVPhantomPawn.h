/*
* Class declaration for pawn class Hackflight flight manager and OpenCV camera display 
* with edge detection
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../SimQuadMixer.hpp"
#include "../../MainModule/vehicles/Phantom.hpp"
#include "../FlightManager.hpp"

#include "../EdgeDetectionCamera.hpp"

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "OpenCVPhantomPawn.generated.h"

UCLASS(Config=Game)
class AOpenCVPhantomPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper classes
        Phantom _phantom;
        SimQuadMixer _mixer;

        FHackflightFlightManager * _flightManager = NULL;

        EdgeDetectionCamera  _camera;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AOpenCVPhantomPawn();

}; // AOpenCVPhantomPawn
