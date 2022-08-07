/*
* Class implementation for DJI Phantom pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
 */

#include "Phantom.h"

APhantom::APhantom()
{
    vehicle.buildFull(this, FrameStatics.mesh.Get());

    // Add propellers
    addRotor(PropCCWStatics.mesh.Get(), +1, +1);
    addRotor(PropCCWStatics.mesh.Get(), -1, -1);
    addRotor(PropCWStatics.mesh.Get(), +1, -1);
    addRotor(PropCWStatics.mesh.Get(), -1, +1);

    _flightManager = NULL;
}

void APhantom::PostInitializeComponents()
{
    vehicle.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void APhantom::BeginPlay()
{
    _flightManager = new FFlightManager(this, &dynamics);

    vehicle.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void APhantom::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    FFlightManager::stopThread(&_flightManager);

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void APhantom::Tick(float DeltaSeconds)
{
    vehicle.Tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}

void APhantom::addCamera(Camera * camera)
{
    vehicle.addCamera(camera);
}

void APhantom::addRotor(UStaticMesh * mesh, int8_t dx, int8_t dy)
{
    vehicle.addRotor(mesh, dx*0.12, dy*0.12, 0.16);
}
