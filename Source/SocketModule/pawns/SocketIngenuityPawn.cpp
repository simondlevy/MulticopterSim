/*
* Class implementation for Ingenuity pawn in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "SocketIngenuityPawn.h"

ASocketIngenuityPawn::ASocketIngenuityPawn()
{
    _ingenuity.build(this);

    _ingenuity.addCamera(&_camera);
}

void ASocketIngenuityPawn::PostInitializeComponents()
{
    _ingenuity.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void ASocketIngenuityPawn::BeginPlay()
{
    _ingenuity.BeginPlay(new FSocketFlightManager(&_ingenuity.dynamics));

    Super::BeginPlay();
}

void ASocketIngenuityPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _ingenuity.EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void ASocketIngenuityPawn::Tick(float DeltaSeconds)
{
    _ingenuity.Tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}


