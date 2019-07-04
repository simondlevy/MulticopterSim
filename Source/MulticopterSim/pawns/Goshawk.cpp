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
DECLARE_STATIC_MESH(FFrameStatics,  "Goshawk/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FLogoStatics, "Goshawk/Logo.Logo",   LogoStatics)

DECLARE_STATIC_MESH(FProp1WStatics, "Goshawk/PropCW.PropCW",   Prop1Statics)
DECLARE_STATIC_MESH(FProp2WStatics, "Goshawk/PropCW.PropCW",   Prop2Statics)
DECLARE_STATIC_MESH(FProp3WStatics, "Goshawk/PropCCW.PropCCW", Prop3Statics)
DECLARE_STATIC_MESH(FProp4WStatics, "Goshawk/PropCCW.PropCCW", Prop4Statics)
DECLARE_STATIC_MESH(FProp5WStatics, "Goshawk/PropCCW.PropCCW", Prop5Statics)
DECLARE_STATIC_MESH(FProp6WStatics, "Goshawk/PropCCW.PropCCW", Prop6Statics)
DECLARE_STATIC_MESH(FProp7WStatics, "Goshawk/PropCW.PropCW",   Prop7Statics)
DECLARE_STATIC_MESH(FProp8WStatics, "Goshawk/PropCW.PropCW",   Prop8Statics)

AGoshawkPawn::AGoshawkPawn()
{
    Vehicle::objects_t objects = {0};

    objects.pawn = this;
    objects.frameMesh = FrameStatics.mesh.Get();
    objects.motorMesh = NULL;

    Vehicle::build(objects);

    // Add our logo
	UStaticMesh * logoMesh = LogoStatics.mesh.Get();
	static ConstructorHelpers::FObjectFinder<UMaterial> decalMaterial(TEXT("'/Game/Flying/Decals/RoboTiCan_Mat'"));
	logoMesh->SetMaterial(0, decalMaterial.Object);
	Vehicle::addMesh(objects, logoMesh, "LogoMesh", 
            FVector(-0.22, 0.005, 0.10),  // location
            FRotator(0,90,100),           // rotation
            FVector(1,0.33,1)*.1875);      // scale

    Vehicle::addMotor(objects, 0, +1.60, +0.60, _layout, Prop1Statics.mesh.Get());
    Vehicle::addMotor(objects, 1, -1.60, -0.60, _layout, Prop2Statics.mesh.Get());
    Vehicle::addMotor(objects, 2, +0.60, +1.50, _layout, Prop3Statics.mesh.Get());
    Vehicle::addMotor(objects, 3, -1.60, +0.60, _layout, Prop4Statics.mesh.Get());
    Vehicle::addMotor(objects, 4, +1.60, -0.60, _layout, Prop5Statics.mesh.Get());
    Vehicle::addMotor(objects, 5, -0.70, -1.50, _layout, Prop6Statics.mesh.Get());
    Vehicle::addMotor(objects, 6, +0.60, -1.50, _layout, Prop7Statics.mesh.Get());
    Vehicle::addMotor(objects, 7, -0.70, +1.50, _layout, Prop8Statics.mesh.Get());

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
