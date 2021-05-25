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
    rft::MockMotor * motors[4] = {&_rotor1, &_rotor2, &_servo1, &_servo2};

    _flightManager = new FHackflightFlightManager(this, &_mixer, motors, &_rocket.dynamics);

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


