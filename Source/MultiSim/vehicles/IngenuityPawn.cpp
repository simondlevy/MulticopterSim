/*
* Class implementation for Ingenuity pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "IngenuityPawn.h"

AIngenuityPawn::AIngenuityPawn()
{
    _ingenuity.build(this);
}

void AIngenuityPawn::PostInitializeComponents()
{
    _ingenuity.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void AIngenuityPawn::BeginPlay()
{
    _flightManager = new FFlightManager(this, &_ingenuity.dynamics);

    _ingenuity.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void AIngenuityPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _ingenuity.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void AIngenuityPawn::Tick(float DeltaSeconds)
{
    _ingenuity.Tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}


