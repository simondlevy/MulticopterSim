/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "TinyWhoop.hpp"

#include "../FlightManager.hpp"

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "TinyWhoopPawn.generated.h"

UCLASS(Config=Game)
class ATinyWhoopPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper classes
        TinyWhoop _tinyWhoop;

        FFlightManager * _flightManager = NULL;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        ATinyWhoopPawn();

}; // ATinyWhoopPawn
