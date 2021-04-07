/*
* Class declaration for target pawn 
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#define WIN32_LEAN_AND_MEAN

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include "../MainModule/Vehicle.hpp"
#include "../MainModule/TargetManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "TargetPawn.generated.h"

UCLASS(Config=Game)
class ATargetPawn : public APawn
{
    friend class FEventCameraManager;

    private:

		GENERATED_BODY()

        UPROPERTY(Category = Mesh, VisibleDefaultsOnly, BlueprintReadOnly, meta = (AllowPrivateAccess = "true"))
            class UStaticMeshComponent* _targetMesh;

        Vehicle _vehicle;

        FTargetManager * _targetManager = NULL;

        UStaticMesh * _frameMesh = NULL;

    protected:

        FBox getBoundingBox(void);

    public:

        virtual void Tick(float DeltaSeconds) override;

        virtual void BeginPlay() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        ATargetPawn();

        ~ATargetPawn();
};
