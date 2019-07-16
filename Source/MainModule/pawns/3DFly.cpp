/*
* Class implementation for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "3DFly.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "3DFly/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FMotorStatics, "3DFly/Motor.Motor", MotorStatics)
DECLARE_STATIC_MESH(FProp1Statics, "3DFly/Prop1.Prop1", Prop1Statics)
DECLARE_STATIC_MESH(FProp2Statics, "3DFly/Prop2.Prop2", Prop2Statics)
DECLARE_STATIC_MESH(FProp3Statics, "3DFly/Prop3.Prop3", Prop3Statics)
DECLARE_STATIC_MESH(FProp4Statics, "3DFly/Prop4.Prop4", Prop4Statics)

static void addMotorAndProp(Vehicle::objects_t & objects, uint8_t index, int8_t x, int8_t y, UStaticMesh * propMesh)
{
    float d = 0.0375;

    UStaticMeshComponent * mMeshComponent = objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(Vehicle::makeName("Motor", index, "Mesh"));
    mMeshComponent->SetStaticMesh(objects.motorMesh);
    mMeshComponent->SetupAttachment(objects.frameMeshComponent, USpringArmComponent::SocketName); 	
    mMeshComponent->AddRelativeLocation(FVector(x*d, y*d-0.01, 0.005)*100); // m => cm

    Vehicle::addProp(objects, index, x*d, y*d, +.025, propMesh);
}

A3DFlyPawn::A3DFlyPawn()
{
    Vehicle::objects_t objects = {0};

    objects.pawn = this;
    objects.frameMesh = FrameStatics.mesh.Get();
    objects.motorMesh = MotorStatics.mesh.Get();

    Vehicle::build(objects);

    addMotorAndProp(objects, 0, +1, +1, Prop1Statics.mesh.Get());
    addMotorAndProp(objects, 1, -1, -1, Prop2Statics.mesh.Get());
    addMotorAndProp(objects, 2, +1, -1, Prop3Statics.mesh.Get());
    addMotorAndProp(objects, 3, -1, +1, Prop4Statics.mesh.Get());

   _vehicle = new Vehicle(objects, new QuadXAPDynamics(_params));
}

A3DFlyPawn::~A3DFlyPawn()
{
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
