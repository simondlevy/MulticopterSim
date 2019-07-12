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

    //extern TargetManager * createTargetManager(void);
    //_manager = createTargetManager();
}

ATargetPawn::~ATargetPawn()
{
    //delete _manager;
}

void ATargetPawn::BeginPlay()
{
	_targetManager = FTargetManager::create();

	Super::BeginPlay();
}

void ATargetPawn::Tick(float DeltaSeconds) 
{
	//static uint32_t _count;
	//debug("%d", ++_count);

    //debug("%s", _manager->getMessage());
    //SetActorLocation(_manager->getLocation() * 100); // m => cm
    //_manager->getLocation();

	debug(_targetManager->getMessage());
}
