/*
* Class implementation for TinyWhoop pawn in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "HackflightTinyWhoopPawn.h"


AHackflightTinyWhoopPawn::AHackflightTinyWhoopPawn()
{
    _tinyWhoop.build(this);
}

void AHackflightTinyWhoopPawn::PostInitializeComponents()
{
    _tinyWhoop.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void AHackflightTinyWhoopPawn::BeginPlay()
{
    _flightManager = new FHackflightFlightManager(this, &_mixer, &_motors, 4, &_tinyWhoop.dynamics);

    _tinyWhoop.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void AHackflightTinyWhoopPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _tinyWhoop.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void AHackflightTinyWhoopPawn::Tick(float DeltaSeconds)
{
    _tinyWhoop.Tick(DeltaSeconds);

    _flightManager->tick();

    Super::Tick(DeltaSeconds);
}


