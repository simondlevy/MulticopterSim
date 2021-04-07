/*
* Class implementation for target pawn
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "TargetPawn.h"
#include "LorenzTargetManager.hpp"

#include "UObject/ConstructorHelpers.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Target/Target.Target", FrameStatics)

ATargetPawn::ATargetPawn()
{
    // Structure to hold one-time initialization
    struct FConstructorStatics
    {
        ConstructorHelpers::FObjectFinderOptional<UStaticMesh> _targetMesh;
        FConstructorStatics() : _targetMesh(TEXT("/Game/Flying/Meshes/Target/Frame.Frame"))
        {
        }
    };
    static FConstructorStatics ConstructorStatics;

    _frameMesh = FrameStatics.mesh.Get();

    _vehicle.build(this, _frameMesh);
}

ATargetPawn::~ATargetPawn()
{
    FThreadedManager::stopThread((FThreadedManager **)&_targetManager);
}

void ATargetPawn::BeginPlay()
{
    _targetManager = new FLorenzTargetManager();

    Super::BeginPlay();
}

void ATargetPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    Super::EndPlay(EndPlayReason);
}

void ATargetPawn::Tick(float DeltaSeconds) 
{
    SetActorLocation(_targetManager->getLocation() * 100); // m => cm
}
        
FBox ATargetPawn::getBoundingBox(void)
{
    return _frameMesh->GetBoundingBox();
}
