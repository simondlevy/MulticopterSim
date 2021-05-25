/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/quads/quadxap.hpp>

#include <rft_motors/mockrotary.hpp>

#include "../../MainModule/vehicles/multirotors/TinyWhoop.hpp"

#include "../HackflightFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "TinyWhoopPawn.generated.h"

UCLASS(Config=Game)
class HACKFLIGHTMODULE_API ATinyWhoopPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        TinyWhoop _tinyWhoop;

        FHackflightFlightManager * _flightManager = NULL;

        rft::MockRotaryMotor _motor1;
        rft::MockRotaryMotor _motor2;
        rft::MockRotaryMotor _motor3;
        rft::MockRotaryMotor _motor4;

        hf::MixerQuadXAP  _mixer = hf::MixerQuadXAP(&_motor1, &_motor2, &_motor3, &_motor4);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        ATinyWhoopPawn();

}; // ATinyWhoopPawn
