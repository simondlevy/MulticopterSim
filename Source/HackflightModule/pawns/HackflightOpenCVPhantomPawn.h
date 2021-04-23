/*
* Class declaration for pawn class Hackflight flight manager and OpenCV camera display 
* with edge detection
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../../MainModule/vehicles/multirotors/Phantom.hpp"

#include "EdgeDetectionCamera.hpp"
#include "NullFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "HackflightOpenCVPhantomPawn.generated.h"

UCLASS(Config=Game)
class HACKFLIGHTMODULE_API AHackflightOpenCVPhantomPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Phantom _phantom;

        EdgeDetectionCamera  _camera;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AHackflightOpenCVPhantomPawn();

}; // AHackflightOpenCVPhantomPawn
