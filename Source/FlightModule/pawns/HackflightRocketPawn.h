/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/thrustvec.hpp>

#include "../../MainModule/vehicles/rockets/Rocket.hpp"

#include "../HackflightFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "HackflightRocketPawn.generated.h"

UCLASS(Config=Game)
class FLIGHTMODULE_API AHackflightRocketPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Rocket _rocket;

        SimMotor _motors = SimMotor(4);

        hf::MixerThrustVector  _mixer = hf::MixerThrustVector(&_motors);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AHackflightRocketPawn();

}; // AHackflightRocketPawn
