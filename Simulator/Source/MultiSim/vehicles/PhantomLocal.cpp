/*
* Class implementation for DJI Phantom pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
 */

#include "PhantomLocal.h"
#include "../threads/LocalThread.hpp"

APhantomLocal::APhantomLocal()
{
    buildPhantom(this, vehicle);
}

// Called when the game starts or when spawned
void APhantomLocal::BeginPlay()
{
    vehicle.beginPlay(new FLocalThread(&dynamics));

    Super::BeginPlay();
}

void APhantomLocal::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    vehicle.endPlay();

    Super::EndPlay(EndPlayReason);
}

void APhantomLocal::PostInitializeComponents()
{
    vehicle.postInitializeComponents();

    Super::PostInitializeComponents();
}

// Called automatically on main thread
void APhantomLocal::Tick(float DeltaSeconds)
{
    vehicle.tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}
