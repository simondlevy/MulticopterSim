/*
* Class implementation for Phantom pawn in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "HackflightPhantomPawn.h"
#include "SimMotor.hpp"

AHackflightPhantomPawn::AHackflightPhantomPawn()
{
    _phantom.build(this);
}

void AHackflightPhantomPawn::PostInitializeComponents()
{
    _phantom.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void AHackflightPhantomPawn::BeginPlay()
{
    _flightManager = new FHackflightFlightManager(this, &_mixer, &_motors, &_phantom.dynamics);

    _phantom.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void AHackflightPhantomPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _phantom.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void AHackflightPhantomPawn::Tick(float DeltaSeconds)
{
    _phantom.Tick(DeltaSeconds);

    _flightManager->tick();

    Super::Tick(DeltaSeconds);
}


