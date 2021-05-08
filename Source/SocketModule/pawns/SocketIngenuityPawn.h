/*
* Class declaration for Ingenuity pawn class using UDP sockets
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../../MainModule/vehicles/multirotors/Ingenuity.hpp"

#include "../SocketFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "SocketIngenuityPawn.generated.h"

UCLASS(Config=Game)
class SOCKETMODULE_API ASocketIngenuityPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Ingenuity _ingenuity;

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

        ASocketIngenuityPawn();

}; // ASocketIngenuityPawn
