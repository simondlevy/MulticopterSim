/*
* Class implementation for big quadcopter pawn class in MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "Goshawk.h"

#include "UObject/ConstructorHelpers.h"
#include "GameFramework/SpringArmComponent.h"

//
// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics,  "Goshawk/Frame.Frame",  FrameStatics)
DECLARE_STATIC_MESH(FProp1WStatics, "Goshawk/PropCCW.PropCCW", Prop1Statics)
DECLARE_STATIC_MESH(FProp2WStatics, "Goshawk/PropCCW.PropCCW", Prop2Statics)
DECLARE_STATIC_MESH(FProp3WStatics, "Goshawk/PropCW.PropCW",  Prop3Statics)
DECLARE_STATIC_MESH(FProp4WStatics, "Goshawk/PropCW.PropCW",  Prop4Statics)

// Constructor
AGoshawkPawn::AGoshawkPawn()
{
    Vehicle::objects_t objects = {0};

    objects.pawn = this;
    objects.frameMesh = FrameStatics.mesh.Get();
    objects.motorMesh = NULL;

    OctoXAP::build(objects, _layout,
            Prop1Statics.mesh.Get(), Prop2Statics.mesh.Get(), Prop3Statics.mesh.Get(), Prop4Statics.mesh.Get()); 

    _vehicle = new OctoXAP(objects, _params);
}

AGoshawkPawn::~AGoshawkPawn()
{
}

void AGoshawkPawn::PostInitializeComponents()
{
    _vehicle->PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void AGoshawkPawn::BeginPlay()
{
    _vehicle->BeginPlay();

    Super::BeginPlay();
}

void AGoshawkPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    _vehicle->EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void AGoshawkPawn::Tick(float DeltaSeconds)
{
    _vehicle->Tick();

    Super::Tick(DeltaSeconds);
}
