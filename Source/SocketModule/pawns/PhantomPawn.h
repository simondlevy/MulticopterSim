/*
* Class declaration for DJI Phantom pawn class using UDP sockets
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../../MainModule/vehicles/multirotors/Phantom.hpp"

#include "../SocketFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "PhantomPawn.generated.h"

UCLASS(Config=Game)
class SOCKETMODULE_API APhantomPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Phantom _phantom;

        // Camera
        SocketCamera _camera;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        APhantomPawn();

}; // APhantomPawn
