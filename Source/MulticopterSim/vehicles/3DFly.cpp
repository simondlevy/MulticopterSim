/*
* Class implementation for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "3DFly.h"
#include "dynamics/QuadXAPDynamics.hpp"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "3DFly/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FMotorStatics, "3DFly/Motor.Motor", MotorStatics)
DECLARE_STATIC_MESH(FProp1Statics, "3DFly/Prop1.Prop1", Prop1Statics)
DECLARE_STATIC_MESH(FProp2Statics, "3DFly/Prop2.Prop2", Prop2Statics)
DECLARE_STATIC_MESH(FProp3Statics, "3DFly/Prop3.Prop3", Prop3Statics)
DECLARE_STATIC_MESH(FProp4Statics, "3DFly/Prop4.Prop4", Prop4Statics)

A3DFlyPawn::A3DFlyPawn()
{
    // The Vehicle object will handle most of the work for the pawn
    _vehicle = new Vehicle(this, FrameStatics.mesh.Get(), new QuadXAPDynamics(&_params), 4);

    // Build the motor and propeller meshes specific to this vehicle

    float cx = 0.00, cy = 0.00, w = 3.75, h = 3.75, mo = -1.00, mz = 0.50, pz = 2.50;

    _vehicle->addMotor(0, -1, TEXT("Motor1Mesh"), MotorStatics.mesh.Get(), FVector(cx+w, cy+h+mo, mz), 
            TEXT("Prop1Mesh"), Prop1Statics.mesh.Get(), FVector(cx+w, cy+h, pz)); 

    _vehicle->addMotor(1, -1, TEXT("Motor2Mesh"), MotorStatics.mesh.Get(), FVector(cx-w, cy-h+mo, mz), 
            TEXT("Prop2Mesh"), Prop2Statics.mesh.Get(), FVector(cx-w, cy-h, pz)); 

    _vehicle->addMotor(2, +1, TEXT("Motor3Mesh"), MotorStatics.mesh.Get(), FVector(cx+w, cy-h+mo, mz),
            TEXT("Prop3Mesh"), Prop3Statics.mesh.Get(), FVector(cx+w, cy-h, pz)); 

    _vehicle->addMotor(3, +1, TEXT("Motor4Mesh"), MotorStatics.mesh.Get(), FVector(cx-w, cy+h+mo, mz),
            TEXT("Prop4Mesh"), Prop4Statics.mesh.Get(), FVector(cx-w, cy+h, pz)); 
}

A3DFlyPawn::~A3DFlyPawn()
{
    delete _vehicle;
}


void A3DFlyPawn::PostInitializeComponents()
{
    _vehicle->PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void A3DFlyPawn::BeginPlay()
{
    _vehicle->BeginPlay();

    Super::BeginPlay();
}

void A3DFlyPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _vehicle->EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void A3DFlyPawn::Tick(float DeltaSeconds)
{
    _vehicle->Tick();

    Super::Tick(DeltaSeconds);
}
