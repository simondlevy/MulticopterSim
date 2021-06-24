/*
* Class implementation for TinyWhoop pawn in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "TinyWhoopPawn.h"


ATinyWhoopPawn::ATinyWhoopPawn()
{
    _tinyWhoop.build(this);
}

void ATinyWhoopPawn::PostInitializeComponents()
{
    _tinyWhoop.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void ATinyWhoopPawn::BeginPlay()
{
    SimMotor * motors[4] = {&_motor1, &_motor2, &_motor3, &_motor4};

    _flightManager = new FHackflightFlightManager(this, &_mixer, motors, &_tinyWhoop.dynamics);

    _tinyWhoop.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void ATinyWhoopPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _tinyWhoop.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void ATinyWhoopPawn::Tick(float DeltaSeconds)
{
    _tinyWhoop.Tick(DeltaSeconds);

    _flightManager->tick();

    Super::Tick(DeltaSeconds);
}


