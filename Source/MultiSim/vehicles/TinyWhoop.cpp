/*
* Class implementation for TinyWhoop pawn in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "TinyWhoop.h"

ATinyWhoop::ATinyWhoop()
{
    // Build the frame
    vehicle.buildFull(this, FrameStatics.mesh.Get());

    // Add propellers
    float x13 = -.0470, x24 = +.0430, y14 = -.020, y23 = +.070;
    addRotor(x13, y14);
    addRotor(x24, y23);
    addRotor(x13, y23);
    addRotor(x24, y14);

    // Add motor barrels
    addMotor(Motor1Statics.mesh.Get(), 1);
    addMotor(Motor2Statics.mesh.Get(), 2);
    addMotor(Motor3Statics.mesh.Get(), 3);
    addMotor(Motor4Statics.mesh.Get(), 4);

    // Add battery, camera, etc.
    vehicle.addMesh(BatteryStatics.mesh.Get(), "BatteryMesh");
    vehicle.addMesh(CameraMountStatics.mesh.Get(), "CameraMountMesh");
    vehicle.addMesh(CameraStatics.mesh.Get(), "CameraMesh");
    vehicle.addMesh(WhoopFCStatics.mesh.Get(), "WhoopFCMesh");
    vehicle.addMesh(Screw1Statics.mesh.Get(), "Screw1Mesh");
    vehicle.addMesh(Screw2Statics.mesh.Get(), "Screw2Mesh");
    vehicle.addMesh(Screw3Statics.mesh.Get(), "Screw3Mesh");
    vehicle.addMesh(Screw4Statics.mesh.Get(), "Screw4Mesh");

    // Flight manager will be set in BeginPlay()
    _flightManager = NULL;
}

// Called when the game starts or when spawned
void ATinyWhoop::BeginPlay()
{
    _flightManager = new FFlightManager(this, &dynamics);

    vehicle.BeginPlay(_flightManager);

    Super::BeginPlay();
}

void ATinyWhoop::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    FFlightManager::stopThread(&_flightManager);

    Super::EndPlay(EndPlayReason);
}

void ATinyWhoop::PostInitializeComponents()
{
    vehicle.PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called automatically on main thread
void ATinyWhoop::Tick(float DeltaSeconds)
{
    vehicle.Tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}

// Adds simulated motor barrel to frame
void ATinyWhoop::addMotor(UStaticMesh * motorMesh, uint8_t id)
{
    char meshName[10];
    SPRINTF(meshName, "Motor%d", id);
    vehicle.addMesh(motorMesh, meshName);
}

void ATinyWhoop::addRotor(float x, float y)
{
    vehicle.addRotor(PropCCWStatics.mesh.Get(), x, y, 0.04);
}

