/*
* Class declaration for Ingenuity pawn class using Hackflight flight manager
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/coaxial.hpp>

#include "../../MainModule/vehicles/Ingenuity.hpp"

#include "../HackflightFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "IngenuityPawn.generated.h"

UCLASS(Config=Game)
class HACKFLIGHTMODULE_API AIngenuityPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Ingenuity _ingenuity;

        FHackflightFlightManager * _flightManager = NULL;

        hf::CoaxialMixer _mixer;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AIngenuityPawn();

}; // IngenuityPawn
