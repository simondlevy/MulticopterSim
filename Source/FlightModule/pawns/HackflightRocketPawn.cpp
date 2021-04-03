/*
* Class implementation for Rocket pawn in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "HackflightRocketPawn.h"

AHackflightRocketPawn::AHackflightRocketPawn()
{
    _rocket.build(this);
}

void AHackflightRocketPawn::PostInitializeComponents()
{
    _rocket.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void AHackflightRocketPawn::BeginPlay()
{
    _rocket.BeginPlay(new FHackflightFlightManager(this, &_mixer, &_motors, 4, _rocket.dynamics));

    Super::BeginPlay();
}

void AHackflightRocketPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _rocket.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void AHackflightRocketPawn::Tick(float DeltaSeconds)
{
    _rocket.Tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}


