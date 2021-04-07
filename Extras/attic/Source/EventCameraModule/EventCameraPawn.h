/*
* Class declaration for pawn class using a simulated event camera
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../MainModule/vehicles/Phantom.h"

#include "HoverFlightManager.hpp"
#include "EventCameraManager.hpp"
#include "TargetPawn.h"

#include "dvssim/cpp/Davis346Display.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "EventCameraPawn.generated.h"

UCLASS(Config=Game)
class FLIGHTMODULE_API AEventCameraPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Phantom _phantom;

        FEventCameraManager * _eventCameraManager = NULL;

        Davis346Display _display;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AEventCameraPawn();

}; // AEventCameraPawn
