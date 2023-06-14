/*
* Class implementation for DJI Phantom pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
 */

#include "PhantomRemote.h"
#include "../threads/remote_control/RemoteControlThread.hpp"

APhantomRemote::APhantomRemote()
{
    buildPhantom(this, vehicle);

    // Un-comment for camera
    // vehicle.addCamera(&camera);
}

// Called when the game starts or when spawned
void APhantomRemote::BeginPlay()
{
    vehicle.beginPlay(new FRemoteControlThread(&dynamics));

    Super::BeginPlay();
}

void APhantomRemote::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    vehicle.endPlay();

    Super::EndPlay(EndPlayReason);
}

void APhantomRemote::PostInitializeComponents()
{
    vehicle.postInitializeComponents();

    Super::PostInitializeComponents();
}

// Called automatically on main thread
void APhantomRemote::Tick(float DeltaSeconds)
{
    vehicle.tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}
