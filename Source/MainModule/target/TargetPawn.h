/*
* Class declaration for target pawn class in MulticopterSim
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

#include "target/TargetManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "TargetPawn.generated.h"

UCLASS(Config=Game)
class ATargetPawn : public APawn
{
    private:

		GENERATED_BODY()

        UPROPERTY(Category = Mesh, VisibleDefaultsOnly, BlueprintReadOnly, meta = (AllowPrivateAccess = "true"))
            class UStaticMeshComponent* _targetMesh;

        //TargetManager * _manager;

    public:

        virtual void Tick(float DeltaSeconds) override;

		virtual void BeginPlay() override;

        ATargetPawn();

        ~ATargetPawn();

private:

	FTargetManager * _targetManager = NULL;
};
