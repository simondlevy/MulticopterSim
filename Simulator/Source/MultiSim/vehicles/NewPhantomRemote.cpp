/*
* Class implementation for DJI Phantom pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
 */

#include "NewPhantomRemote.h"
#include "../threads/NewRemoteThread.hpp"

ANewPhantomRemote::ANewPhantomRemote()
{
    buildPhantom(this, vehicle);

    // Un-comment for camera
    // vehicle.addCamera(&camera);
}

// Called when the game starts or when spawned
void ANewPhantomRemote::BeginPlay()
{
    vehicle.beginPlay(new FNewRemoteThread(&dynamics));

    Super::BeginPlay();
}

void ANewPhantomRemote::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    vehicle.endPlay();

    Super::EndPlay(EndPlayReason);
}

void ANewPhantomRemote::PostInitializeComponents()
{
    vehicle.postInitializeComponents();

    Super::PostInitializeComponents();
}

// Called automatically on main thread
void ANewPhantomRemote::Tick(float DeltaSeconds)
{
    vehicle.tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}
