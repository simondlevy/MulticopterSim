/*
* Class implementation for Phantom pawn in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "NullPhantomPawn.h"

ANullPhantomPawn::ANullPhantomPawn()
{
    _phantom.build(this);
}

void ANullPhantomPawn::PostInitializeComponents()
{
    _phantom.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void ANullPhantomPawn::BeginPlay()
{
    _phantom.BeginPlay(new FNullFlightManager(&_phantom.dynamics));

    Super::BeginPlay();
}

void ANullPhantomPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _phantom.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void ANullPhantomPawn::Tick(float DeltaSeconds)
{
    _phantom.Tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}


