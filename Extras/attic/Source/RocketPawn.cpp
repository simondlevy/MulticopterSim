/*
* Class implementation for Rocket pawn in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "RocketPawn.h"

ARocketPawn::ARocketPawn()
{
    _rocket.build(this);
}

void ARocketPawn::PostInitializeComponents()
{
    _rocket.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void ARocketPawn::BeginPlay()
{
    _flightManager = new FHackflightFlightManager(
            /*this,
            &_mixer,
            _motors,*/
            &_rocket.dynamics);

    _rocket.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void ARocketPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _rocket.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void ARocketPawn::Tick(float DeltaSeconds)
{
    _rocket.Tick(DeltaSeconds);

    _flightManager->tick();

    Super::Tick(DeltaSeconds);
}


