/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/quadxap.hpp>

#include "../../MainModule/vehicles/multirotors/TinyWhoop.h"

#include "../HackflightFlightManager.hpp"

#include "SimMotor.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "HackflightTinyWhoopPawn.generated.h"

UCLASS(Config=Game)
class FLIGHTMODULE_API AHackflightTinyWhoopPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        TinyWhoop _tinyWhoop;

        SimMotor _motors = SimMotor(4);

        hf::MixerQuadXAP  _mixer = hf::MixerQuadXAP(&_motors);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AHackflightTinyWhoopPawn();

}; // AHackflightTinyWhoopPawn
