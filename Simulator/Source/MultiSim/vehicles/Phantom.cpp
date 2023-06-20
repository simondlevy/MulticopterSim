/*
* Class implementation for DJI Phantom pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
 */

#include "Phantom.h"
#include "../threads/RemoteControlThread.hpp"

APhantom::APhantom()
{
    buildPhantom(this, vehicle);

    // Un-comment for camera
    // vehicle.addCamera(&camera);
}

// Called when the game starts or when spawned
void APhantom::BeginPlay()
{
    vehicle.beginPlay(new FRemoteControlThread(&dynamics));

    Super::BeginPlay();
}

void APhantom::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    vehicle.endPlay();

    Super::EndPlay(EndPlayReason);
}

void APhantom::PostInitializeComponents()
{
    vehicle.postInitializeComponents();

    Super::PostInitializeComponents();
}

// Called automatically on main thread
void APhantom::Tick(float DeltaSeconds)
{
    vehicle.tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}
