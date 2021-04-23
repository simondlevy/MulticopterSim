/*
* Class implementation for Phantom pawn in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "HackflightOpenCVPhantomPawn.h"

AHackflightOpenCVPhantomPawn::AHackflightOpenCVPhantomPawn()
{
    _phantom.build(this);

    _phantom.addCamera(&_camera);
}

void AHackflightOpenCVPhantomPawn::PostInitializeComponents()
{
    _phantom.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void AHackflightOpenCVPhantomPawn::BeginPlay()
{
    _flightManager = new FHackflightFlightManager(this, &_mixer, &_motors, &_phantom.dynamics);

    _phantom.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void AHackflightOpenCVPhantomPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _phantom.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void AHackflightOpenCVPhantomPawn::Tick(float DeltaSeconds)
{
    _phantom.Tick(DeltaSeconds);

    _flightManager->tick();

    Super::Tick(DeltaSeconds);
}


