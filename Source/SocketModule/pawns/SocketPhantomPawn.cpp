/*
* Class implementation for Phantom pawn in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "SocketPhantomPawn.h"

ASocketPhantomPawn::ASocketPhantomPawn()
{
    _phantom.build(this);

    _phantom.addCamera(&_camera);
}

void ASocketPhantomPawn::PostInitializeComponents()
{
    _phantom.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void ASocketPhantomPawn::BeginPlay()
{
    _phantom.BeginPlay(new FSocketFlightManager(&_phantom.dynamics));

    Super::BeginPlay();
}

void ASocketPhantomPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _phantom.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void ASocketPhantomPawn::Tick(float DeltaSeconds)
{
    _phantom.Tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}


