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
DECLARE_STATIC_MESH(FFrameStatics, "Goshawk/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FNameplateStatics,  "Goshawk/Nameplate.Nameplate",   NameplateStatics)

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
	UStaticMesh * logoMesh = NameplateStatics.mesh.Get();
	/*
	static ConstructorHelpers::FObjectFinder<UMaterial> decalMaterial(TEXT("'/Game/Flying/Decals/RoboTiCan_Mat'"));
	logoMesh->SetMaterial(0, decalMaterial.Object);
	Vehicle::addMesh(objects, logoMesh, "NameplateMesh", 
            FVector(-0.22, 0.005, 0.10),  // location
            FRotator(0,90,100),           // rotation
            FVector(1,0.33,1)*.1875);      // scale
			*/

    static constexpr float LO = 0.22;
    static constexpr float HI = 0.52;

    addProp(objects, 0, +HI, +LO, Prop1Statics.mesh.Get());
    addProp(objects, 1, -HI, -LO, Prop2Statics.mesh.Get());
    addProp(objects, 2, +LO, +HI, Prop3Statics.mesh.Get());
    addProp(objects, 3, -HI, +LO, Prop4Statics.mesh.Get());
    addProp(objects, 4, +HI, -LO, Prop5Statics.mesh.Get());
    addProp(objects, 5, -LO, -HI, Prop6Statics.mesh.Get());
    addProp(objects, 6, +LO, -HI, Prop7Statics.mesh.Get());
    addProp(objects, 7, -LO, +HI, Prop8Statics.mesh.Get());

    _vehicle = new Vehicle(objects, new QuadXAPDynamics(_params));
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
