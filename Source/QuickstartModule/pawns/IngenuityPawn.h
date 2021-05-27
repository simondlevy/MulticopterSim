/*
* Class declaration for Ingenuity pawn class using UDP sockets
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../../MainModule/vehicles/multirotors/Ingenuity.hpp"

#include "../QuickstartFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "IngenuityPawn.generated.h"

UCLASS(Config=Game)
class QUICKSTARTMODULE_API AIngenuityPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Ingenuity _ingenuity;

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AIngenuityPawn();

}; // AIngenuityPawn
