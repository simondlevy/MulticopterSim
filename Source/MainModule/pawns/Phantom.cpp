/*
* Class implementation for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "Phantom.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Phantom/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FProp1Statics, "Phantom/Prop.Prop", Prop1Statics)
DECLARE_STATIC_MESH(FProp2Statics, "Phantom/Prop.Prop", Prop2Statics)
DECLARE_STATIC_MESH(FProp3Statics, "Phantom/Prop.Prop", Prop3Statics)
DECLARE_STATIC_MESH(FProp4Statics, "Phantom/Prop.Prop", Prop4Statics)

APhantomPawn::APhantomPawn()
{
    Vehicle::objects_t objects = {0};

    objects.pawn = this;

    objects.frameMesh = FrameStatics.mesh.Get();

    Vehicle::build(objects);

    Vehicle::addProp(objects, 0, +0.12, +0.12, +0.15, Prop1Statics.mesh.Get());
    Vehicle::addProp(objects, 1, -0.12, -0.12, +0.15, Prop2Statics.mesh.Get());
    Vehicle::addProp(objects, 2, +0.12, -0.12, +0.15, Prop3Statics.mesh.Get());
    Vehicle::addProp(objects, 3, -0.12, +0.12, +0.15, Prop4Statics.mesh.Get());

    _vehicle = new QuadXAP(objects, _params);
}

APhantomPawn::~APhantomPawn()
{
}


void APhantomPawn::PostInitializeComponents()
{
    _vehicle->PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void APhantomPawn::BeginPlay()
{
    _vehicle->BeginPlay();

    Super::BeginPlay();
}

void APhantomPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _vehicle->EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void APhantomPawn::Tick(float DeltaSeconds)
{
    _vehicle->Tick();

    Super::Tick(DeltaSeconds);
}
