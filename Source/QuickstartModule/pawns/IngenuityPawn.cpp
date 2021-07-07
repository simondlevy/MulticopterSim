/*
* Class implementation for Ingenuity pawn in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "SocketModule/pawns/IngenuityPawn.h"

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
    _ingenuity.BeginPlay(new FQuickstartFlightManager(&_ingenuity.dynamics));

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


