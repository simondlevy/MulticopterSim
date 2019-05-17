/*
* Class implementation for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "VehiclePawn.h"
#include "Debug.h"

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

    // Allocate space for motor values used in animation/sound
    _motorvals = new double[getMotorCount()];
}

AVehiclePawn::~AVehiclePawn()
{
    delete _motorvals;
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
            for (uint8_t j=0; j<getMotorCount(); ++j) {
                if (child->GetName() == getPropellerMeshNames()[j]) {
                    _propMeshes[j] = child;
                }
            }
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

    if (_mapSelected) {

        // Start the audio for the propellers Note that because the
        // Cue Asset is set to loop the sound, once we start playing the sound, it
        // will play continiously...
        _propellerAudioComponent->Play();

        // Get vehicle ground-truth location and rotation to initialize flight manager
        FVector pos = this->GetActorLocation() / 100; // cm => m
        double groundTruthPosition[3] = {pos.X, pos.Y, pos.Z};
        FRotator rot = this->GetActorRotation(); 
        double groundTruthRotation[3] = {rot.Roll, rot.Pitch, rot.Yaw};

        // Launch a new threaded flight manager 
        _flightManager = FFlightManager::createFlightManager(this, groundTruthPosition, groundTruthRotation);
    }

    else {
        error("NO MAP SELECTED");
    }

    Super::BeginPlay();
}

void AVehiclePawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    // Stop the flight controller
    if (_mapSelected) {

        // Stop threaded dymamics worker and free its memory
        _flightManager = (FFlightManager *)FThreadedWorker::stopThreadedWorker(_flightManager);
    }

    Super::EndPlay(EndPlayReason);
}

void AVehiclePawn::Tick(float DeltaSeconds)
{
    if (_mapSelected) {

        getPoseAndMotors(DeltaSeconds);

        addAnimationEffects();

        debug("%s", _flightManager->getMessage());
    }

    Super::Tick(DeltaSeconds);
}

void AVehiclePawn::getPoseAndMotors(float deltaT)
{
    // Get current pose kinematics and motor values from flight manager. Motor
    // values are used only for animation effects (prop rotation, sound).
    double position[3] = {0};
    double rotation[3] = {0};
    _flightManager->getPoseAndMotors(deltaT, position, rotation, _motorvals);

    // XXX avoid bugs pose values (shouldn't be arising to begin with )
    if (sanityCheck(position) && sanityCheck(rotation)) {

        SetActorLocation(FVector(position[0], position[1], position[2]) * 100); // m =>cm

        // Note rotation order: pitch, yaw, roll = 1,2,0 = Y,Z,X)
        SetActorRotation(FRotator(rotation[1], rotation[2], rotation[0]) * (180 / M_PI)); // radians => deg
    }
}

bool AVehiclePawn::sanityCheck(double v[3])
{
    for (uint8_t j=0; j<3; ++j) {
        if (abs(v[j]) > 1e6) {
            return true;
        }
    }

    return true;
}

void AVehiclePawn::addAnimationEffects(void)
{
    // Compute the mean of the motor values
    float motormean = 0;
    for (uint8_t j=0; j<getMotorCount(); ++j) {
        motormean += _motorvals[j];
    }
    motormean /= getMotorCount();

    // Use the mean motor value to modulate the pitch and voume of the propeller sound
    setAudioPitchAndVolume(motormean);

    // Rotate props. For visual effect, we can ignore actual motor values, and just keep increasing the rotation.
    if (motormean > 0) {
        static float rotation;
        for (uint8_t j=0; j<getMotorCount(); ++j) {
            _propMeshes[j]->SetRelativeRotation(FRotator(0,  rotation * getMotorDirection(j)*100, 0));
        }
        rotation++;
    }
}

void AVehiclePawn::setAudioPitchAndVolume(float value)
{
    _propellerAudioComponent->SetFloatParameter(FName("pitch"), value);
    _propellerAudioComponent->SetFloatParameter(FName("volume"), value);
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

void AVehiclePawn::setGimbal(float roll, float pitch, float yaw)
{

    FRotator rotation = _fpvSpringArm->GetComponentRotation();

    rotation.Roll  += roll;
    rotation.Pitch -= pitch;
    rotation.Yaw   += yaw;

    _fpvSpringArm->SetWorldRotation(rotation);
}

