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

<<<<<<< HEAD
=======
// Debugging
static const FColor TEXT_COLOR = FColor::Yellow;
static const float  TEXT_SCALE = 2.f;

// Scaling constant for turning motor spin to thrust
static const float THRUST_FACTOR = 130;

>>>>>>> 808d5ef1b9f6a83ef264dbaf484a8cfcddba9f48
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

    // Initialize the motor-spin values
    for (uint8_t k=0; k<4; ++k) {
        _motorvals[k] = 0;
    }

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
	_accelZ = 0;
	_elapsedTime = 1.0; // avoid divide-by-zero
    _propIndex = 0;

	// Initialize sensors
	//_rangefinder.init();


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

    // Update the flight controller with the current IMU readings
	float gyro[3] = { _gyro.X, _gyro.Y, _gyro.Z  }; 
	FVector location = this->GetActorLocation() / 100; // cm to m
	float position[3] = { location.X, location.Y, location.Z };
	FVector velocity = this->GetVelocity() / 100; // cm/s to ms/c
	float vel[3] = { velocity.X, velocity.Y, velocity.Z };
    float quat[4] = {_quat.W, _quat.X, _quat.Y, _quat.Z};
    float accel[3] = {0,0,0};
    getAccelerometer(accel);
	_flightController->update(_elapsedTime, position, vel, quat, gyro, accel, _motorvals);
	//debug("%+3.3f %+3.3f %+3.3f", accel[0], accel[1], accel[2]);

    // Compute body-frame roll, pitch, yaw velocities based on differences between motors
    float forces[3];
    _vehiclePhysics->computeAngularForces(_motorvals, forces);

    // Rotate vehicle
    //AddActorLocalRotation(DeltaSeconds * FRotator(forces[1], forces[2], forces[0]) * (180 / M_PI));

    // Sum over motor values to get overall thrust
    float motorSum = 0;
    for (uint8_t k=0; k<4; ++k) {
        motorSum += _motorvals[k];
    }

    // Rotate one prop per tick
    FRotator PropRotation(0, _motorvals[_propIndex]*motordirs[_propIndex]*240, 0);
    //PropMeshes[_propIndex]->AddLocalRotation(PropRotation);
    _propIndex = (_propIndex+1) % 4;

    // Get current quaternion and convert it to our format (XXX)
    _quat = this->GetActorQuat();
    _quat.X = -_quat.X;
    _quat.Y = -_quat.Y;

    // Convert quaternion to Euler angles
    FVector euler = this->getEulerAngles();

    // Use Euler angle first difference to emulate gyro
    _gyro = (euler - _eulerPrev) / DeltaSeconds;
    _eulerPrev = euler;

    // Use velocity first difference to emulate accelerometer
    float vario = this->GetVelocity().Z / 100; // m/s
    _accelZ = (vario - _varioPrev) / DeltaSeconds;
    _varioPrev = vario;

    debug("%+3.3f", _accelZ);

    // Rotate Euler angles into inertial frame: http://www.chrobotics.com/library/understanding-euler-angles
    float x = sin(euler.X)*sin(euler.Z) + cos(euler.X)*cos(euler.Z)*sin(euler.Y);
    float y = cos(euler.X)*sin(euler.Y)*sin(euler.Z) - cos(euler.Z)*sin(euler.X);
    float z = cos(euler.Y)*cos(euler.X);

    // Add movement force to vehicle 
    //PlaneMesh->AddForce(motorSum*THRUST_FACTOR*FVector(-x, -y, z));

    // Modulate the pitch and voume of the propeller sound
    propellerAudioComponent->SetFloatParameter(FName("pitch"), motorSum / 4);
    propellerAudioComponent->SetFloatParameter(FName("volume"), motorSum / 4);

	// Accumulate elapsed time
	_elapsedTime += DeltaSeconds;

    // Call any parent class Tick implementation
    Super::Tick(DeltaSeconds);
}

void AVehiclePawn::NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
        bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit)
{
    Super::NotifyHit(MyComp, Other, OtherComp, bSelfMoved, HitLocation, HitNormal, NormalImpulse, Hit);

    // Deflect along the surface when we collide.
    FRotator CurrentRotation = GetActorRotation();
    SetActorRotation(FQuat::Slerp(CurrentRotation.Quaternion(), HitNormal.ToOrientationQuat(), 0.025f));
}

void AVehiclePawn::getAccelerometer(float accelGs[3])
{
	// Get Euler angles
	FVector euler = FMath::DegreesToRadians(this->GetActorQuat().Euler());

 	// Slide 50 from https://slideplayer.com/slide/2813564/

 	float phi   = euler.X;
	float theta = euler.Y;

 	accelGs[0] = -sin(theta);
	accelGs[1] =  sin(phi)*cos(theta);
	accelGs[2] =  cos(phi)*cos(theta);
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
    // on screen
    if (GEngine) {

        // -1 = no overwrite (0 for overwrite); 5.f = arbitrary time to display; true = newer on top
        GEngine->AddOnScreenDebugMessage(0, 5.f, TEXT_COLOR, FString(buf), true, FVector2D(TEXT_SCALE,TEXT_SCALE));
    }

    // on Visual Studio output console
    OutputDebugStringA(buf);
}

// Helper methods ---------------------------------------------------------------------------------

FVector AVehiclePawn::getEulerAngles(void)
{
    return FMath::DegreesToRadians(this->GetActorQuat().Euler());
}

