/*
* VehiclePawn.cpp: Class implementation for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "VehiclePawn.h"
#include "UObject/ConstructorHelpers.h"
#include "Camera/CameraComponent.h"
#include "Components/StaticMeshComponent.h"
#include "Components/InputComponent.h"
#include "GameFramework/SpringArmComponent.h"
#include "GameFramework/CharacterMovementComponent.h"
#include "Engine.h"
#include "Engine/World.h"
#include "Engine/StaticMesh.h"
#include "Runtime/Core/Public/Math/UnrealMathUtility.h"
#include "Debug.h"

#include <cmath>
#include <stdarg.h>

AVehiclePawn::AVehiclePawn()
{
	// Structure to hold one-time initialization
	struct FConstructorStatics
	{
		ConstructorHelpers::FObjectFinderOptional<UStaticMesh> _vehicleMesh;
		FConstructorStatics() : _vehicleMesh(TEXT("/Game/Flying/Meshes/3DFly.3DFly"))
		{
		}
	};
	static FConstructorStatics ConstructorStatics;

	// Create static mesh component
	_vehicleMesh = CreateDefaultSubobject<UStaticMeshComponent>(TEXT("PlaneMesh0"));
	_vehicleMesh->SetStaticMesh(ConstructorStatics._vehicleMesh.Get());	// Set static mesh
	RootComponent = _vehicleMesh;

    // Create physics support
    _physics = Physics::createPhysics(this);

    // Turn off UE4 physics
	_vehicleMesh->SetSimulatePhysics(false);

	// Load our Sound Cue for the propeller sound we created in the editor... 
	// note your path may be different depending
	// on where you store the asset on disk.
	static ConstructorHelpers::FObjectFinder<USoundCue> propellerCue(TEXT("'/Game/Flying/Audio/MotorSoundCue'"));
	
	// Store a reference to the Cue asset - we'll need it later.
	_propellerAudioCue = propellerCue.Object;

	// Create an audio component, the audio component wraps the Cue, 
	// and allows us to ineract with it, and its parameters from code.
	_propellerAudioComponent = CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

	// Stop the sound from sound playing the moment it's created.
	_propellerAudioComponent->bAutoActivate = false;

	// Attach the sound to the pawn's root, the sound follows the pawn around
	_propellerAudioComponent->SetupAttachment(GetRootComponent());

    // Set up the FPV camera
    _fpvSpringArm = CreateDefaultSubobject<USpringArmComponent>(TEXT("fpvSpringArm"));
    _fpvSpringArm->SetupAttachment(RootComponent);
    _fpvSpringArm->TargetArmLength = 0.f; // The camera follows at this distance behind the character
    _fpvCamera = CreateDefaultSubobject<UCameraComponent>(TEXT("fpvCamera"));
    _fpvCamera ->SetupAttachment(_fpvSpringArm, USpringArmComponent::SocketName); 	
}

void AVehiclePawn::PostInitializeComponents()
{
	if (_propellerAudioCue->IsValidLowLevelFast()) {
		_propellerAudioComponent->SetSound(_propellerAudioCue);
	}

    // Grab the static prop mesh components by name, storing them for use in Tick()
    TArray<UStaticMeshComponent *> staticComponents;
    this->GetComponents<UStaticMeshComponent>(staticComponents);
    for (int i = 0; i < staticComponents.Num(); i++) {
        if (staticComponents[i]) {
            UStaticMeshComponent* child = staticComponents[i];
            if (child->GetName() == "Prop1") _propMeshes[0] = child;
            if (child->GetName() == "Prop2") _propMeshes[1] = child;
            if (child->GetName() == "Prop3") _propMeshes[2] = child;
            if (child->GetName() == "Prop4") _propMeshes[3] = child;
        }
	}

	Super::PostInitializeComponents();
}
// Called when the game starts or when spawned
void AVehiclePawn::BeginPlay()
{
    // Make sure a map has been selected
	FString mapName = GetWorld()->GetMapName();
	_mapSelected = !mapName.Contains("Untitled");

    // Start the physics and start playing the sound.  Note that because the
    // Cue Asset is set to loop the sound, once we start playing the sound, it
    // will play continiously...
	if (_mapSelected) {
		_propellerAudioComponent->Play();
        _physics->start();
	}
    else {
        debug("NO MAP SELECTED");
    }

	Super::BeginPlay();
}

void AVehiclePawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    // Stop the flight controller
    if (_mapSelected) {
        _physics->stop();
    }

    Super::EndPlay(EndPlayReason);
}

void AVehiclePawn::Tick(float DeltaSeconds)
{
	// D'oh!
	if (!_mapSelected) {
		Super::Tick(DeltaSeconds);
		return;
	}

    // Update physics, getting back motor values for animation effects
	TArray<float> motorvals = _physics->update(DeltaSeconds);

    // Add animation effects (prop rotation, sound)
    addAnimationEffects(motorvals);

	Super::Tick(DeltaSeconds);
}

void AVehiclePawn::addAnimationEffects(TArray<float> motorvals)
{
    float motormean = mean(motorvals);

    // Modulate the pitch and voume of the propeller sound
	setAudioPitchAndVolume(motormean);

    // For visual effect, we can ignore actual motor values, and just keep increasing the rotation
    static float rotation;

    // Rotate props
    if (motormean > 0) {
        for (uint8_t k = 0; k < 4; ++k) {
            _propMeshes[k]->SetRelativeRotation(FRotator(0,  rotation * MOTORDIRS[k] * 100, 0));
        }
    }

    rotation++;
}

void AVehiclePawn::setAudioPitchAndVolume(float value)
{
    _propellerAudioComponent->SetFloatParameter(FName("pitch"), value);
    _propellerAudioComponent->SetFloatParameter(FName("volume"), value);
}

float AVehiclePawn::mean(TArray<float> x)
{
	float mn = 0;

	for (auto It = x.CreateConstIterator(); It; ++It) {
		mn += *It;
	}

	return mn / x.Num();
}


void AVehiclePawn::NotifyHit(
        class UPrimitiveComponent* MyComp, 
        class AActor* Other, 
        class UPrimitiveComponent* OtherComp, 
        bool bSelfMoved, 
        FVector HitLocation, 
        FVector HitNormal, 
        FVector NormalImpulse, 
        const FHitResult& Hit)
{
    Super::NotifyHit(MyComp, Other, OtherComp, bSelfMoved, HitLocation, HitNormal, NormalImpulse, Hit);

    // Deflect along the surface when we collide.
    //FRotator CurrentRotation = GetActorRotation();
    //SetActorRotation(FQuat::Slerp(CurrentRotation.Quaternion(), HitNormal.ToOrientationQuat(), 0.025f));
}

float AVehiclePawn::getCurrentTime(void)
{
    return UGameplayStatics::GetRealTimeSeconds(GetWorld());
}
        
void AVehiclePawn::setGimbal(float roll, float pitch, float yaw)
{

	FRotator rotation = _fpvSpringArm->GetComponentRotation();

    rotation.Roll  += roll;
    rotation.Pitch -= pitch;
    rotation.Yaw   += yaw;

	_fpvSpringArm->SetWorldRotation(rotation);
}

