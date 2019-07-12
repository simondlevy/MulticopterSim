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
		FConstructorStatics() : _targetMesh(TEXT("/Game/Flying/Meshes/Target/Target.Target"))
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
    //delete _manager;
}

void ATargetPawn::BeginPlay()
{
	extern FTargetManager * createTargetManager(void);
	_targetManager = createTargetManager();

	Super::BeginPlay();
}

void ATargetPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
	delete _targetManager;

	Super::EndPlay(EndPlayReason);
}

void ATargetPawn::Tick(float DeltaSeconds) 
{
    SetActorLocation(_targetManager->getLocation() * 100); // m => cm

	debug("%s", _targetManager->getMessage());
}
