/*
* Class implementation for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "BigQuad.h"

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

ABigQuadPawn::ABigQuadPawn()
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
        camera1TextureObject(TEXT("/Game/Flying/RenderTargets/camera1RenderTarget"));

    // Get texture object from each render target
    _camera1RenderTarget = camera1TextureObject.Object;

    // Set up the FPV camera

    FVector cameraScale(0.1, 0.1, 0.1);

    _gimbalSpringArm = CreateDefaultSubobject<USpringArmComponent>(TEXT("gimbalSpringArm"));
    _gimbalSpringArm->SetupAttachment(RootComponent);
    _gimbalSpringArm->TargetArmLength = 0.f; // The camera follows at this distance behind the character

    _camera1 = CreateDefaultSubobject<UCameraComponent>(TEXT("camera1"));
    _camera1 ->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName); 	
    _camera1->SetWorldScale3D(cameraScale);
    _camera1->SetFieldOfView(90);
    _camera1->SetAspectRatio(4./3);

    _capture1 = CreateDefaultSubobject<USceneCaptureComponent2D >(TEXT("capture1"));
    _capture1->SetWorldScale3D(cameraScale);
    _capture1->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName);
    _capture1->TextureTarget = _camera1RenderTarget;
    _capture1->FOVAngle = 45;

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


    // Allocate space for motor values used in animation/sound
    _motorvals = new float[4];

    // Create dynamics
    _dynamics = new QuadXAPDynamics(&_params, _motorLocations);
}

ABigQuadPawn::~ABigQuadPawn()
{
    delete _motorvals;
}

bool ABigQuadPawn::childComponentHasName(UStaticMeshComponent * child, const char * fmt, int index)
{
    char name[100] = {0};
    sprintf_s(name, fmt, index+1);
    return child->GetName() == (const char *)name;
}


void ABigQuadPawn::PostInitializeComponents()
{
    // Add "Vehicle" tag for use by level blueprint
    this->Tags.Add(FName("Vehicle"));

    if (_propellerAudioCue->IsValidLowLevelFast()) {
        _propellerAudioComponent->SetSound(_propellerAudioCue);
    }

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void ABigQuadPawn::BeginPlay()
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

void ABigQuadPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    if (_mapSelected) {

        stopThreadedWorkers();
    }

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void ABigQuadPawn::Tick(float DeltaSeconds)
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
#ifdef _USE_OPENCV
        _videoManager->grabImage();
#endif

        // OSD for debugging messages from threaded workers
        debug("%s", _flightManager->getMessage());
    }

    // Switch cameras periodically for testing
    switchCameras(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}


void ABigQuadPawn::switchCameras(float DeltaSeconds)
{
#ifdef _USE_OPENCV
    static bool useCamera2;
    static float time;
    static float prevTime;
    time += DeltaSeconds;
    if (time - prevTime > 2) {
        prevTime = time;
        useCamera2 = !useCamera2;
        if (useCamera2) {
            //_videoManager->useCamera2();
        }
        else {
            //_videoManager->useCamera1();
        }
    }
#endif
}

void ABigQuadPawn::startThreadedWorkers(void)
{
    _flightManager = FFlightManager::create(_dynamics, _startLocation, _startRotation);
#ifdef _USE_OPENCV
    _videoManager  = FVideoManager::create(_camera1RenderTarget);
#endif
}

void ABigQuadPawn::stopThreadedWorkers(void)
{
    _flightManager = (FFlightManager *)FThreadedWorker::stopThreadedWorker(_flightManager);
#ifdef _USE_OPENCV
    _videoManager  = (FVideoManager *)FThreadedWorker::stopThreadedWorker(_videoManager);
#endif
}

void ABigQuadPawn::getKinematics(void)
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

void ABigQuadPawn::addAnimationEffects(void)
{
    // Compute the mean of the motor values
    float motormean = 0;
    for (uint8_t j=0; j<4; ++j) {
        motormean += _motorvals[j];
    }
    motormean /= 4;

    // Use the mean motor value to modulate the pitch and voume of the propeller sound
    setAudioPitchAndVolume(motormean);
}

void ABigQuadPawn::setAudioPitchAndVolume(float value)
{
    _propellerAudioComponent->SetFloatParameter(FName("pitch"), value);
    _propellerAudioComponent->SetFloatParameter(FName("volume"), value);
}

void ABigQuadPawn::NotifyHit(
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

void ABigQuadPawn::setGimbal(void)
{
    // Get gimbal location from flight manager
    float roll = 0, pitch = 0;
    _flightManager->getGimbal(roll, pitch);

    FRotator rotation = _gimbalSpringArm->GetComponentRotation();

    rotation.Roll  += roll;
    rotation.Pitch -= pitch;

    _gimbalSpringArm->SetWorldRotation(rotation);
}

