/*
* Class implementation for Ingenuity pawn in MulticopterSim
*
* Copyright (C) 2021 Simon D. Levy
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
    rft::MockMotor * motors[4] = {&_rotor1, &_rotor2, &_servo1, &_servo2};

    _flightManager = new FHackflightFlightManager(this, &_mixer, motors, &_ingenuity.dynamics);

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

    _flightManager->tick();

    Super::Tick(DeltaSeconds);
}


