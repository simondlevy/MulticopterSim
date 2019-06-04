/*
* Class implementation for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "VehiclePawn.h"

#include "Debug.hpp"

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

static const wchar_t * FRAME_MESH_NAME = TEXT("/Game/Flying/Meshes/BigQuad/Frame.Frame");

AVehiclePawn::AVehiclePawn()
{
	// Structure to hold one-time initialization
	struct FConstructorStatics
	{
		ConstructorHelpers::FObjectFinderOptional<UStaticMesh> _vehicleMesh;
		FConstructorStatics() : _vehicleMesh(FRAME_MESH_NAME)
		{
		}
	};
	static FConstructorStatics ConstructorStatics;

	// Create static mesh component
	_vehicleMesh = CreateDefaultSubobject<UStaticMeshComponent>(TEXT("PlaneMesh0"));
	_vehicleMesh->SetStaticMesh(ConstructorStatics._vehicleMesh.Get());
	RootComponent = _vehicleMesh;
    
    // Accessing camera render targets from map is done statically (at compile time).
    static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>
        camera1TextureObject(TEXT("/Game/Flying/RenderTargets/fpv1CameraTarget"));
    static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>
        camera2TextureObject(TEXT("/Game/Flying/RenderTargets/fpv2CameraTarget"));

    // Get texture object from each render target
    _camera1RenderTarget = camera1TextureObject.Object;
    _camera2RenderTarget = camera2TextureObject.Object;

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

    FVector cameraScale(0.1, 0.1, 0.1);

    _gimbalSpringArm = CreateDefaultSubobject<USpringArmComponent>(TEXT("gimbalSpringArm"));
    _gimbalSpringArm->SetupAttachment(RootComponent);
    _gimbalSpringArm->TargetArmLength = 0.f; // The camera follows at this distance behind the character

    _gimbalCamera = CreateDefaultSubobject<UCameraComponent>(TEXT("gimbalCamera"));
    _gimbalCamera ->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName); 	
    _gimbalCamera->SetWorldScale3D(cameraScale);
    _gimbalCamera->SetFieldOfView(90);
    _gimbalCamera->SetAspectRatio(4./3);

    _gimbalCapture = CreateDefaultSubobject<USceneCaptureComponent2D >(TEXT("gimbalCapture"));
    _gimbalCapture->SetWorldScale3D(cameraScale);
    _gimbalCapture->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName);
    _gimbalCapture->TextureTarget = _camera1RenderTarget;
    _gimbalCapture->FOVAngle = 45;

    // Allocate space for motor values used in animation/sound
    _motorvals = new float[_frame.motorCount()];
}

AVehiclePawn::~AVehiclePawn()
{
    delete _motorvals;
}

bool AVehiclePawn::childComponentHasName(UStaticMeshComponent * child, const char * fmt, int index)
{
    char name[100] = {0};
    sprintf_s(name, fmt, index+1);
    return child->GetName() == (const char *)name;
}


void AVehiclePawn::PostInitializeComponents()
{
    if (_propellerAudioCue->IsValidLowLevelFast()) {
        _propellerAudioComponent->SetSound(_propellerAudioCue);
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

        // Get vehicle ground-truth location and rotation to initialize flight manager, now and after any crashes
        _startLocation = this->GetActorLocation();
        _startRotation = this->GetActorRotation(); 

        // Initialize threaded workers
        startThreadedWorkers();
    }

    else {
        error("NO MAP SELECTED");
    }

    Super::BeginPlay();
}

void AVehiclePawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    if (_mapSelected) {

        stopThreadedWorkers();
    }

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void AVehiclePawn::Tick(float DeltaSeconds)
{
    // A hack to avoid accessing kinematics before dynamics thread is ready
    static uint64_t count;

    if (_mapSelected && count++>10) {

        // Kinematics from dynamics
        getKinematics();

        // Keepin' it real(istic)!
        addAnimationEffects();

        // Move gimbal
        setGimbal();

        // Tell the threaded video work to grab the current camera image
        _videoManager->grabCurrentImage();

        // OSD for debugging messages from threaded workers
        debug("%s", _flightManager->getMessage());
        //debug("%s", _videoManager->getMessage());
    }

    Super::Tick(DeltaSeconds);
}


void AVehiclePawn::startThreadedWorkers(void)
{
    _flightManager = FFlightManager::create(&_frame, _startLocation, _startRotation);
    _videoManager  = FVideoManager::create(_camera1RenderTarget, NULL);
}

void AVehiclePawn::stopThreadedWorkers(void)
{
    _flightManager = (FFlightManager *)FThreadedWorker::stopThreadedWorker(_flightManager);
    _videoManager  = (FVideoManager *)FThreadedWorker::stopThreadedWorker(_videoManager);
}

void AVehiclePawn::getKinematics(void)
{
    // Get current pose kinematics and motor values dynamics (from flight
    // manager). Motor values are used only for animation effects (prop
    // rotation, sound).
    FVector location;
    FRotator rotation;
    bool crashed = _flightManager->getKinematics(location, rotation, _motorvals);

    if (crashed) {

        // Restart threaded workers
        stopThreadedWorkers();
        startThreadedWorkers();
    }

    SetActorLocation(location);
    SetActorRotation(rotation);
}

void AVehiclePawn::addAnimationEffects(void)
{
    // Compute the mean of the motor values
    float motormean = 0;
    for (uint8_t j=0; j<_frame.motorCount(); ++j) {
        motormean += _motorvals[j];
    }
    motormean /= _frame.motorCount();

    // Use the mean motor value to modulate the pitch and voume of the propeller sound
    setAudioPitchAndVolume(motormean);
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
}

void AVehiclePawn::setGimbal(void)
{
    // Get gimbal location from flight manager
    float roll = 0, pitch = 0;
    _flightManager->getGimbal(roll, pitch);

    FRotator rotation = _gimbalSpringArm->GetComponentRotation();

    rotation.Roll  += roll;
    rotation.Pitch -= pitch;

    _gimbalSpringArm->SetWorldRotation(rotation);
}

