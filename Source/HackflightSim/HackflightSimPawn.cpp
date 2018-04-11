// Copyright 1998-2017 Epic Games, Inc. All Rights Reserved.

#include "HackflightSimPawn.h"
#include "UObject/ConstructorHelpers.h"
#include "Camera/CameraComponent.h"
#include "Components/StaticMeshComponent.h"
#include "Components/InputComponent.h"
#include "GameFramework/SpringArmComponent.h"
#include "Engine/World.h"
#include "Engine/StaticMesh.h"
#include "Runtime/Core/Public/Math/UnrealMathUtility.h"

// Main firmware
hf::Hackflight hackflight;

// Controller input
#ifdef _WIN32
#include <receivers/sim/windows.hpp>
#else
#include <receivers/sim/linux.hpp>
#endif
hf::Controller controller;

// Debugging

static FColor TEXT_COLOR = FColor::Yellow;
static float  TEXT_SCALE = 2.f;

void hf::Board::outbuf(char * buf)
{
	if (GEngine) {

		// 0 = overwrite; 5.0f = arbitrary time to display
		GEngine->AddOnScreenDebugMessage(0, 5.0f, TEXT_COLOR, FString(buf), true, FVector2D(TEXT_SCALE,TEXT_SCALE));
	}

}

// PID tuning
hf::Stabilizer stabilizer = hf::Stabilizer(
	1.0f,      // Level P
	.00001f,    // Gyro cyclic P
	0,			// Gyro cyclic I
	0,			// Gyro cyclic D
	0,			// Gyro yaw P
	0);			// Gyro yaw I


// Pawn methods ---------------------------------------------------

AHackflightSimPawn::AHackflightSimPawn()
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

	// Start Hackflight firmware
	hackflight.init(this, &controller, &stabilizer);

    // Initialize the motor-spin values
    for (uint8_t k=0; k<4; ++k) {
        motorvals[k] = 0;
    }

    // Initialize elapsed time
    elapsedTime = 0;

	// Load our Sound Cue for the propeller sound we created in the editor... 
	// note your path may be different depending
	// on where you store the asset on disk.
	static ConstructorHelpers::FObjectFinder<USoundCue> propellerCue(TEXT("'/Game/Flying/Audio/MotorSoundCue'"));
	
	// Store a reference to the Cue asset - we'll need it later.
	propellerAudioCue = propellerCue.Object;

	// Create an audio component, the audio component wraps the Cue, 
	// and allows us to ineract with it, and its parameters from code.
	propellerAudioComponent = CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

	// I don't want the sound playing the moment it's created.
	propellerAudioComponent->bAutoActivate = false;

	// I want the sound to follow the pawn around, so I attach it to the Pawns root.
	propellerAudioComponent->SetupAttachment(GetRootComponent());

    // Set up the FPV camera
    fpvSpringArm = CreateDefaultSubobject<USpringArmComponent>(L"FpvSpringArm");
    fpvSpringArm->SetupAttachment(RootComponent);
    fpvCamera = CreateDefaultSubobject<UCameraComponent>(L"FpvCamera");
    fpvCamera ->SetupAttachment(fpvSpringArm, USpringArmComponent::SocketName); 
}

void AHackflightSimPawn::PostInitializeComponents()
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

void AHackflightSimPawn::BeginPlay()
{
    // Start playing the sound.  Note that because the Cue Asset is set to loop the sound,
    // once we start playing the sound, it will play continiously...
    propellerAudioComponent->Play();

    Super::BeginPlay();
}

void AHackflightSimPawn::Tick(float DeltaSeconds)
{
    // Update our flight firmware
    hackflight.update();

    // Accumulate elapsed time
    elapsedTime += DeltaSeconds;

    // Compute body-frame roll, pitch, yaw velocities based on differences between motors
    float forces[3];
    forces[0] = motorsToAngularForce(2, 3, 0, 1);
    forces[1] = motorsToAngularForce(1, 3, 0, 2); 
    forces[2] = motorsToAngularForce(1, 2, 0, 3); 

    // Rotate vehicle
    AddActorLocalRotation(DeltaSeconds * FRotator(forces[1], forces[2], forces[0]) * (180 / M_PI));

    // Spin props proportionate to motor values, acumulating their sum 
    float motorSum = 0;
    for (uint8_t k=0; k<4; ++k) {
        FRotator PropRotation(0, motorvals[k]*motordirs[k]*60, 0);
        PropMeshes[k]->AddLocalRotation(PropRotation);
        motorSum += motorvals[k];
    }

    // Get current quaternion
    FQuat q = this->GetActorQuat();

    // Convert quaternion to Euler angles
    FVector euler = FMath::DegreesToRadians(q.Euler());

    // Rename Euler X,Y,Z to familiar Greek-letter variables
    float phi   = euler.X;
    float theta = euler.Y;
    float psi   = euler.Z;

    // Rotate Euler angles into inertial frame
    // https://ocw.mit.edu/courses/mechanical-engineering/2-017j-design-of-electromechanical-robotic-systems-fall-2009/course-text/MIT2_017JF09_ch09.pdf
    float x = sin(phi)*sin(psi) + cos(phi)*cos(psi)*sin(theta);
    float y = cos(phi)*sin(theta)*sin(psi) - cos(psi)*sin(phi);
    float z = cos(theta)*cos(phi);

    // Add movement force to vehicle
    PlaneMesh->AddForce(100*motorSum*FVector(-x, -y, z));

    // Modulate the pitch and voume of the propeller sound
    propellerAudioComponent->SetFloatParameter(FName("pitch"), motorSum / 4);
    propellerAudioComponent->SetFloatParameter(FName("volume"), motorSum / 4);

    // Call any parent class Tick implementation
    Super::Tick(DeltaSeconds);
}

void AHackflightSimPawn::NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
        bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit)
{
    Super::NotifyHit(MyComp, Other, OtherComp, bSelfMoved, HitLocation, HitNormal, NormalImpulse, Hit);

    // Deflect along the surface when we collide.
    FRotator CurrentRotation = GetActorRotation();
    SetActorRotation(FQuat::Slerp(CurrentRotation.Quaternion(), HitNormal.ToOrientationQuat(), 0.025f));
}

float AHackflightSimPawn::motorsToAngularForce(int a, int b, int c, int d)
{
    float v = ((motorvals[a] + motorvals[b]) - (motorvals[c] + motorvals[d]));

    return (v<0 ? -1 : +1) * pow(fabs(v), 3);
}

void AHackflightSimPawn::init(void)
{
}

bool AHackflightSimPawn::getEulerAngles(float eulerAngles[3]) 
{
    eulerAngles[0] = eulerAngles[1] = eulerAngles[2] = 0;
    return true;
}

bool AHackflightSimPawn::getGyroRates(float gyroRates[3]) 
{
    gyroRates[0] = gyroRates[1] = gyroRates[2] = 0;
    return true;
}

uint32_t AHackflightSimPawn::getMicroseconds() 
{
    return (uint32_t)(elapsedTime*1e6);
}

void AHackflightSimPawn::writeMotor(uint8_t index, float value) 
{
    motorvals[index] = value;
}
