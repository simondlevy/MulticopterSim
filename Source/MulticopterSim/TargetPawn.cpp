/*
* Class implementation for target pawn class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "TargetPawn.h"
#include "UObject/ConstructorHelpers.h"
#include "Debug.hpp"

ATargetPawn::ATargetPawn()
{
	// Structure to hold one-time initialization
	struct FConstructorStatics
	{
		ConstructorHelpers::FObjectFinderOptional<UStaticMesh> _targetMesh;
		FConstructorStatics() : _targetMesh(TEXT("/Game/Flying/Meshes/Target.Target"))
		{
		}
	};
	static FConstructorStatics ConstructorStatics;

	// Create static mesh component
	_targetMesh = CreateDefaultSubobject<UStaticMeshComponent>(TEXT("TargetMesh0"));
	_targetMesh->SetStaticMesh(ConstructorStatics._targetMesh.Get());	// Set static mesh
	RootComponent = _targetMesh;
}

ATargetPawn::~ATargetPawn()
{
}


void ATargetPawn::Tick(float DeltaSeconds) 
{
    AddActorLocalOffset(FVector(0,0,1), true);
}
