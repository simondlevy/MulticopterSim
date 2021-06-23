/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/thrustvec.hpp>

#include "../../MainModule/vehicles/Rocket.hpp"

#include "../HackflightFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "RocketPawn.generated.h"

UCLASS(Config=Game)
class HACKFLIGHTMODULE_API ARocketPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Rocket _rocket;

        FHackflightFlightManager * _flightManager = NULL;

        // hf::MixerThrustVector _mixer;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        ARocketPawn();

}; // ARocketPawn
