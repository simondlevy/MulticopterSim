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

#include <shlwapi.h>

#include <cmath>
#include <stdarg.h>

AVehiclePawn::AVehiclePawn()
{
	// Structure to hold one-time initialization
	struct FConstructorStatics
	{
		ConstructorHelpers::FObjectFinderOptional<UStaticMesh> PlaneMesh;
		FConstructorStatics()
			: PlaneMesh(TEXT("/Game/Flying/Meshes/3DFly.3DFly"))
		{
		}
	};
	static FConstructorStatics ConstructorStatics;

	// Create static mesh component
	PlaneMesh = CreateDefaultSubobject<UStaticMeshComponent>(TEXT("PlaneMesh0"));
	PlaneMesh->SetStaticMesh(ConstructorStatics.PlaneMesh.Get());	// Set static mesh
	RootComponent = PlaneMesh;

    // Create flight-control support
    _flightController = SimFlightController::createSimFlightController();

	// Load our Sound Cue for the propeller sound we created in the editor... 
	// note your path may be different depending
	// on where you store the asset on disk.
	static ConstructorHelpers::FObjectFinder<USoundCue> propellerCue(TEXT("'/Game/Flying/Audio/MotorSoundCue'"));
	
	// Store a reference to the Cue asset - we'll need it later.
	propellerAudioCue = propellerCue.Object;

	// Create an audio component, the audio component wraps the Cue, 
	// and allows us to ineract with it, and its parameters from code.
	propellerAudioComponent = CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

	// Stop the sound from sound playing the moment it's created.
	propellerAudioComponent->bAutoActivate = false;

	// Attach the sound to the pawn's root, the sound follows the pawn around
	propellerAudioComponent->SetupAttachment(GetRootComponent());

    // Set up the FPV camera
    fpvSpringArm = CreateDefaultSubobject<USpringArmComponent>(L"FpvSpringArm");
    fpvSpringArm->SetupAttachment(RootComponent);
	fpvSpringArm->TargetArmLength = 0.f; // The camera follows at this distance behind the character
    fpvCamera = CreateDefaultSubobject<UCameraComponent>(L"FpvCamera");
    fpvCamera ->SetupAttachment(fpvSpringArm, USpringArmComponent::SocketName); 

    // Set up the physics models
    _vehiclePhysics = VehiclePhysics::createVehiclePhysics();
}

AVehiclePawn::~AVehiclePawn()
{
    delete _vehiclePhysics;
}
	
void AVehiclePawn::PostInitializeComponents()
{
	if (propellerAudioCue->IsValidLowLevelFast()) {
		propellerAudioComponent->SetSound(propellerAudioCue);
	}

    // Grab the static prop mesh components by name, storing them for use in Tick()
    TArray<UStaticMeshComponent *> staticComponents;
    this->GetComponents<UStaticMeshComponent>(staticComponents);
    for (int i = 0; i < staticComponents.Num(); i++) {
        if (staticComponents[i]) {
            UStaticMeshComponent* child = staticComponents[i];
            if (child->GetName() == "Prop1") PropMeshes[0] = child;
            if (child->GetName() == "Prop2") PropMeshes[1] = child;
            if (child->GetName() == "Prop3") PropMeshes[2] = child;
            if (child->GetName() == "Prop4") PropMeshes[3] = child;
        }
	}

	Super::PostInitializeComponents();
}

void AVehiclePawn::BeginPlay()
{
    // Start the flight controller
    _flightController->start();
    
    // Start playing the sound.  Note that because the Cue Asset is set to loop the sound,
    // once we start playing the sound, it will play continiously...
    propellerAudioComponent->Play();

    // Initialize simulation variables
    _eulerPrev = FVector(0, 0, 0);
	_varioPrev = 0;
	_elapsedTime = 1.0; // avoid divide-by-zero
    _propIndex = 0;

    // Check whether benchmarking
    _benchmarking = GetWorld()->GetMapName().Contains(TEXT("Benchmark"));

    Super::BeginPlay();
}

void AVehiclePawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    // Stop the flight controller
    _flightController->stop();

    Super::EndPlay(EndPlayReason);
}

void AVehiclePawn::Tick(float DeltaSeconds)
{
    //debug("%d FPS", (uint16_t)(1/DeltaSeconds));

    // Convert quaternion to Euler angles
    FVector euler = FMath::DegreesToRadians(this->GetActorQuat().Euler());

    // Get the simulated IMU readings
    FVector accel = getAccelerometer(DeltaSeconds);
    FVector gyro  = getGyrometer(euler, DeltaSeconds);
    FQuat   quat  = getQuaternion();

    // Send state to flight controller, dividing by 100 to convert cm to m
    TArray<float> motorvals = _flightController->update(_elapsedTime, GetActorLocation()/100, GetVelocity()/100, quat, gyro, accel);

    // Use physics model to compute rotation and translation forces on vehicle
    FVector rotationForce = {0,0,0};
    FVector translationForce = {0,0,0};
    _vehiclePhysics->computeForces(DeltaSeconds, motorvals, euler, rotationForce, translationForce);

    // Turn off sound and controlled movement in benchmark mode
    if (_benchmarking) {

        debug("Acceleromter X: %+3.3f    Y: %+3.3f    Z: %+3.3f", accel.X, accel.Y, accel.Z);

        silenceAudio();
    }

    else {

        // Add movement force vector to vehicle 
        PlaneMesh->AddForce(translationForce);

        // Add rotation to vehicle 
        AddActorLocalRotation(DeltaSeconds * FRotator(rotationForce.Y, rotationForce.Z, rotationForce.X) * (180 / M_PI));

        // Add animation effects (prop rotation, sound)
        addAnimationEffects(motorvals);
    }

    // Accumulate elapsed time
    _elapsedTime += DeltaSeconds;

    // Call any parent class Tick implementation
    Super::Tick(DeltaSeconds);
}

void AVehiclePawn::addAnimationEffects(TArray<float> motorvals)
{
    // Modulate the pitch and voume of the propeller sound
    setAudioPitchAndVolume(motorvals.Max());

    // Rotate one prop per tick
    FRotator PropRotation(0, motorvals[_propIndex]*MOTORDIRS[_propIndex]*240, 0);
    PropMeshes[_propIndex]->AddLocalRotation(PropRotation);
    _propIndex = (_propIndex+1) % 4;
}

void AVehiclePawn::silenceAudio(void)
{
    setAudioPitchAndVolume(0);
}

void AVehiclePawn::setAudioPitchAndVolume(float value)
{
    propellerAudioComponent->SetFloatParameter(FName("pitch"), value);
    propellerAudioComponent->SetFloatParameter(FName("volume"), value);
}

void AVehiclePawn::NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
        bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit)
{
    Super::NotifyHit(MyComp, Other, OtherComp, bSelfMoved, HitLocation, HitNormal, NormalImpulse, Hit);

    // Deflect along the surface when we collide.
    FRotator CurrentRotation = GetActorRotation();
    SetActorRotation(FQuat::Slerp(CurrentRotation.Quaternion(), HitNormal.ToOrientationQuat(), 0.025f));
}

FVector AVehiclePawn::getAccelerometer(float DeltaSeconds)
{
    // Get Euler angles from quaternion
    FVector euler = FMath::DegreesToRadians(this->GetActorQuat().Euler());

    // Use velocity first difference to emulate G force on vehicle in inertial frame
    float vario = GetVelocity().Z / 100; // m/s
    float gs = ((vario - _varioPrev) / DeltaSeconds + G) / G;
    _varioPrev = vario;

    // Convert inertial frame to body frame
    // See slide 50 from https://slideplayer.com/slide/2813564/
    float phi   = euler.X;
    float theta = euler.Y;
    return gs * FVector(-sin(theta), sin(phi)*cos(theta), cos(phi)*cos(theta));
}

FVector AVehiclePawn::getGyrometer(FVector & euler, float DeltaSeconds)
{
    // Use Euler angle first difference to emulate gyro
    FVector gyro = (euler - _eulerPrev) / DeltaSeconds;
    _eulerPrev = euler;
    return gyro;
}

FQuat AVehiclePawn::getQuaternion(void)
{
    // Get current quaternion and convert it to our format (XXX necessary?
    FQuat quat = this->GetActorQuat();
    quat.X = -quat.X;
    quat.Y = -quat.Y;
    return quat;
}

void AVehiclePawn::debug(char * fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    char buf[200];
    vsnprintf(buf, 200, fmt, ap); 
    va_end(ap);

    outbuf(buf);
}

void AVehiclePawn::outbuf(char * buf)
{
    // Text properties for debugging
    FColor TEXT_COLOR = FColor::Yellow;
    constexpr float  TEXT_SCALE = 2.f;

    // on screen
    if (GEngine) {

        // -1 = no overwrite (0 for overwrite); 5.f = arbitrary time to display; true = newer on top
        GEngine->AddOnScreenDebugMessage(0, 5.f, TEXT_COLOR, FString(buf), true, FVector2D(TEXT_SCALE,TEXT_SCALE));
    }

    // on Visual Studio output console
    OutputDebugStringA(buf);
}
