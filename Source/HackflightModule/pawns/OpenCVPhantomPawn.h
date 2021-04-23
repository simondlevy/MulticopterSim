/*
* Class declaration for pawn class using stubbed flight manager and OpenCV camera display
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../../MainModule/vehicles/multirotors/Phantom.hpp"

#include "EdgeDetectionCamera.hpp"
#include "NullFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "OpenCVPhantomPawn.generated.h"

UCLASS(Config=Game)
class HACKFLIGHTMODULE_API AOpenCVPhantomPawn : public APawn {

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

        AOpenCVPhantomPawn();

}; // AOpenCVPhantomPawn
