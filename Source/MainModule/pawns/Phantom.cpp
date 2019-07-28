/*
* Class implementation for Phantom pawn in MulticopterSim
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

static void addProp(Vehicle::objects_t & objects, uint8_t index, int8_t x, int8_t y, UStaticMesh * propMesh)
{
    float d = 0.12;

    //Vehicle::addProp(objects, index, x*d, y*d, +.15, propMesh);
}

APhantomPawn::APhantomPawn()
{
    Vehicle::objects_t objects = {0};

    objects.pawn = this;

    objects.frameMesh = FrameStatics.mesh.Get();

    //Vehicle::build(objects);

    // Add propellers
    addProp(objects, 0, +1, +1, Prop1Statics.mesh.Get());
    addProp(objects, 1, -1, -1, Prop2Statics.mesh.Get());
    addProp(objects, 2, +1, -1, Prop3Statics.mesh.Get());
    addProp(objects, 3, -1, +1, Prop4Statics.mesh.Get());

    _vehicle = new Vehicle(objects, new QuadXAPDynamics(_params));
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
    _vehicle->BeginPlay(NULL);

    Super::BeginPlay();
}

void APhantomPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void APhantomPawn::Tick(float DeltaSeconds)
{
    _vehicle->Tick();

    Super::Tick(DeltaSeconds);
}
