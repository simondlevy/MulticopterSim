/*
* Class implementation for  Crazyflie in MultiSim
*
* Copyright (C) 2023 Simon D. Levy
*
* MIT License
 */

#include "Crazyflie.h"

#include "../Thread.hpp" 

ACrazyflie::ACrazyflie()
{
    // Build the frame, restoring for cameras, audio
    vehicle.buildFull(this, PcbStatics.mesh.Get(), 0.5, 0.25);

    vehicle.addComponent(BatteryStatics.mesh.Get(), 
            makeMeshName("battery"));

    vehicle.addComponent(BatteryHolderStatics.mesh.Get(),
            makeMeshName("battery_holder"));

    vehicle.addComponent(PinHeadersLeftStatics.mesh.Get(),
            makeMeshName("pin_headers_left"));

    vehicle.addComponent(PinHeadersRightStatics.mesh.Get(),
            makeMeshName("pin_headers_right"));

    addArm(1,
            MotorMount1Statics.mesh.Get(),
            Motor1Statics.mesh.Get(),
            PropCCWStatics.mesh.Get(), +0.063, +0.065);

    addArm(2,
            MotorMount2Statics.mesh.Get(),
            Motor2Statics.mesh.Get(),
            PropCWStatics.mesh.Get(), -0.068, +0.068);

    addArm(3,
            MotorMount3Statics.mesh.Get(),
            Motor3Statics.mesh.Get(),
            PropCCWStatics.mesh.Get(), -0.060, -0.060);

    addArm(4,
            MotorMount4Statics.mesh.Get(),
            Motor4Statics.mesh.Get(),
            PropCWStatics.mesh.Get(), +0.060, -0.060);
}

void ACrazyflie::addArm(
        uint8_t index,
        UStaticMesh * motorMountMesh,
        UStaticMesh * motorMesh,
        UStaticMesh * propellerMesh,
        const float propellerX,
        const float propellerY)
{
    vehicle.addComponent(motorMountMesh, makeMeshName("motor_mount", index));

    vehicle.addComponent(motorMesh, makeMeshName("motor", index));

    vehicle.addRotor(propellerMesh, propellerX, propellerY, 0.0275);
}


// Called when the game starts or when spawned
void ACrazyflie::BeginPlay()
{
    vehicle.beginPlay(new FVehicleThread(&dynamics));

    Super::BeginPlay();
}

void ACrazyflie::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    vehicle.endPlay();

    Super::EndPlay(EndPlayReason);
}

void ACrazyflie::PostInitializeComponents()
{
    vehicle.postInitializeComponents();

    Super::PostInitializeComponents();
}

// Called automatically on main thread
void ACrazyflie::Tick(float DeltaSeconds)
{
    vehicle.tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}
