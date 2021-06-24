/*
* Class declaration for pawn class Hackflight flight manager and OpenCV camera display 
* with edge detection
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/quad/xap.hpp>

#include "../../MainModule/vehicles/Phantom.hpp"

#include "../HackflightFlightManager.hpp"
#include "EdgeDetectionCamera.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "OpenCVPhantomPawn.generated.h"

UCLASS(Config=Game)
class HACKFLIGHTMODULE_API AOpenCVPhantomPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Phantom _phantom;

        FHackflightFlightManager * _flightManager = NULL;

        SimRotaryMotor _motor1;
        SimRotaryMotor _motor2;
        SimRotaryMotor _motor3;
        SimRotaryMotor _motor4;

        hf::MixerQuadXAP _mixer = hf::MixerQuadXAP(&_motor1, &_motor2, &_motor3, &_motor4);

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
