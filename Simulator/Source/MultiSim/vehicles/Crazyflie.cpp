/*
* Class implementation for  Crazyflie in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
 */

#include "Crazyflie.h"

#include "../threads/RemoteThread.hpp" // XXX 

ACrazyflie::ACrazyflie()
{
    // Build the frame, restoring for cameras, audio
    vehicle.buildFull(this, PcbStatics.mesh.Get());

    vehicle.addComponent(BatteryStatics.mesh.Get(),
            makeName("battery", 1, "Mesh"));
    vehicle.addComponent(BatteryHolderStatics.mesh.Get(),
            makeName("battery_holder", 1, "Mesh"));

    vehicle.addComponent(PinHeadersRightStatics.mesh.Get(),
            makeName("pin_headers_right", 1, "Mesh"));

    vehicle.addComponent(PinHeadersLeftStatics.mesh.Get(),
            makeName("pin_headers_left", 1, "Mesh"));

    addArm(1,
            MotorMount1Statics.mesh.Get(),
            Motor1Statics.mesh.Get(),
            PropCCWStatics.mesh.Get(), +0.3125, +0.325);

    addArm(2,
            MotorMount4Statics.mesh.Get(),
            Motor4Statics.mesh.Get(),
            PropCWStatics.mesh.Get(), -0.335, +0.340);

    addArm(3,
            MotorMount3Statics.mesh.Get(),
            Motor3Statics.mesh.Get(),
            PropCCWStatics.mesh.Get(), -0.355, -0.345);

    addArm(4,
            MotorMount2Statics.mesh.Get(),
            Motor2Statics.mesh.Get(),
            PropCWStatics.mesh.Get(), +0.335, -0.345); // 335
}

void ACrazyflie::addArm(
        uint8_t index,
        UStaticMesh * motorMountMesh,
        UStaticMesh * motorMesh,
        UStaticMesh * propellerMesh,
        const float propellerX,
        const float propellerY)
{
    vehicle.addComponent(motorMountMesh, 
            makeName("motor_mount", index, "Mesh"));

    vehicle.addComponent(motorMesh, 
            makeName("motor", index, "Mesh"));

    vehicle.addRotor(propellerMesh, propellerX, propellerY, 0.14);
}


// Called when the game starts or when spawned
void ACrazyflie::BeginPlay()
{
    vehicle.beginPlay(new FRemoteThread(&dynamics));

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
