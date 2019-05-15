/*
* VehiclePawn.cpp: Class implementation for pawn class in MulticopterSim
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

    // Create physics support
    //_physics = Physics::createPhysics(this);

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
        startPhysics();
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
        stopPhysics();
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

    //debug("%s", _dynamicsWorker->getMessage());

    // Update physics, getting back motor values for animation effects
	TArray<float> motorvals = updatePhysics(DeltaSeconds);

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

void AVehiclePawn::startPhysics(void)
{
    // Create a new flight manager (e.g., HackflightSim)
    _flightManager = FlightManager::createFlightManager(this);

    // Create vehicle dynamics via factory method
    _dynamics = MultirotorDynamics::create();

    // Get vehicle ground-truth location and rotation to initialize dynamics
    FVector pos = this->GetActorLocation() / 100; // cm => m
    double groundTruthPosition[3] = {pos.X, pos.Y, pos.Z};
    FRotator rot = this->GetActorRotation(); 
    double groundTruthRotation[3] = {rot.Roll, rot.Pitch, rot.Yaw};
    _dynamics->init(groundTruthPosition, groundTruthRotation);

    // Launch a threaded dynamics worker
    _dynamicsWorker = new FDynamicsWorker(this, _dynamics);
}

void AVehiclePawn::stopPhysics(void)
{
    // Stop threaded dymamics worker and free its memory
    _dynamicsWorker = (FDynamicsWorker *)FThreadedWorker::stopThreadedWorker(_dynamicsWorker);

    delete _dynamics;

    delete _flightManager;
}

TArray<float> AVehiclePawn::updatePhysics(float deltaT)
{
    // Current motor values
    static double _motorvals[4];

    // Get vehicle state by passing motor values to dynamics
    double angularVelocityRPY[3] = {0}; // body frame
    double eulerAngles[3] = {0};        // body frame
    double velocityXYZ[3] = {0};        // inertial frame
    double positionXYZ[3] = {0};        // inertial frame
    _dynamics->update( deltaT, _motorvals, angularVelocityRPY, eulerAngles, velocityXYZ, positionXYZ);

    // Set pawn location using position from dynamics
    this->SetActorLocation(FVector(positionXYZ[0], positionXYZ[1], positionXYZ[2]) * 100); // m =>cm

    // Set pawn rotation using Euler angles (note order: pitch, yaw, roll = 1,2,0 = Y,Z,X)
    this->SetActorRotation(FRotator(eulerAngles[1], eulerAngles[2], eulerAngles[0]) * (180 / M_PI)); // radians => deg

    // Convert Euler angles to quaternion
    double imuOrientationQuat[4]={0};
    MultirotorDynamics::eulerToQuaternion(eulerAngles, imuOrientationQuat);

    // PID controller: update the flight manager with the quaternion and gyrometer, getting the resulting motor values
    // Note quaternion order: https://api.unrealengine.com/INT/API/Runtime/Core/Math/FQuat/__ctor/7/index.html    
    TArray<float> motorvals = _flightManager->update(
            deltaT, 
            FQuat(imuOrientationQuat[1], imuOrientationQuat[2], imuOrientationQuat[3], imuOrientationQuat[0]), 
            FVector(angularVelocityRPY[0], angularVelocityRPY[1], angularVelocityRPY[2]));

    // Set motor values for dynamics on next iteration
    for (uint8_t j=0; j<4; ++j) {
        _motorvals[j] = motorvals[j];
    }

    // Return the motor values for audiovisual effect
    return motorvals;
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

