/*
* HackflightSimPawn.cpp: Class implementation for pawn class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "HackflightSimPawn.h"
#include "UObject/ConstructorHelpers.h"
#include "Camera/CameraComponent.h"
#include "Components/StaticMeshComponent.h"
#include "Components/InputComponent.h"
#include "GameFramework/SpringArmComponent.h"
#include "Engine.h"
#include "Engine/World.h"
#include "Engine/StaticMesh.h"
#include "Runtime/Core/Public/Math/UnrealMathUtility.h"

// Main firmware
hf::Hackflight hackflight;

#ifdef _WIN32
#include "HackflightSimReceiverWindows.h"
#include "ThreadedSocketServer.h"
#else
#include "HackflightSimReceiverLinux.h"
#endif

// MSP comms
#include "msppg/MSPPG.h"

// Controller
hf::Controller controller;

// Socket comms
static const char * HOST = "137.113.118.68";  // thales.cs.wlu.edu
static const int PORT = 20000;
ThreadedSocketServer server = ThreadedSocketServer(PORT, HOST);

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
	1.0f,       // Level P
	.00001f,    // Gyro cyclic P
	0,			// Gyro cyclic I
	0,			// Gyro cyclic D
	0,			// Gyro yaw P
	0);			// Gyro yaw I

// Mixer
#include <mixers/quadx.hpp>
MixerQuadX mixer;


// APawn methods ---------------------------------------------------

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
	hackflight.init(this, &controller, &stabilizer, &mixer);

    // Initialize the motor-spin values
    for (uint8_t k=0; k<4; ++k) {
        motorvals[k] = 0;
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

    // Reset previous Euler angles for gyro emulation
    eulerPrev = FVector(0, 0, 0);

	// Start the server
    serverRunning = true;
	if (!server.start()) {
        serverError();
        serverRunning = false;
    }

    Super::BeginPlay();
}

void AHackflightSimPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
	// Stop the server
	if (serverRunning) {
        if (!server.stop()) {
            serverError();
        }
        else {
            hf::Debug::printf("Disconnected");
        }
    }

	Super::EndPlay(EndPlayReason);
}


void AHackflightSimPawn::Tick(float DeltaSeconds)
{
    // Update our flight firmware
    hackflight.update();

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
    quat = this->GetActorQuat();

    // Convert quaternion to Euler angles
    FVector euler = FMath::DegreesToRadians(quat.Euler());

    // Use Euler angle first difference to emulate gyro
    gyro = (euler - eulerPrev) / DeltaSeconds;
    eulerPrev = euler;
 
    // Rotate Euler angles into inertial frame: http://www.chrobotics.com/library/understanding-euler-angles
    float x = sin(euler.X)*sin(euler.Z) + cos(euler.X)*cos(euler.Z)*sin(euler.Y);
    float y = cos(euler.X)*sin(euler.Y)*sin(euler.Z) - cos(euler.Z)*sin(euler.X);
    float z = cos(euler.Y)*cos(euler.X);

    // Add movement force to vehicle
    PlaneMesh->AddForce(100*motorSum*FVector(-x, -y, z));

    // Modulate the pitch and voume of the propeller sound
    propellerAudioComponent->SetFloatParameter(FName("pitch"), motorSum / 4);
    propellerAudioComponent->SetFloatParameter(FName("volume"), motorSum / 4);

    // Debug status of client connection
	if (!server.connected() && serverRunning) {
		hf::Debug::printf("Listening for connection");
	}

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

    return (v<0 ? -1 : +1) * fabs(v);
}

void AHackflightSimPawn::serverError(void)
{
    hf::Debug::printf("MSP server error: %s", server.lastError());
}

// Hackflight::Board methods ---------------------------------------------------

bool AHackflightSimPawn::getQuaternion(float q[4]) 
{
    q[0] = +quat.W;
    q[1] = -quat.X;
    q[2] = -quat.Y;
    q[3] = +quat.Z;

    return true;
}

bool AHackflightSimPawn::getGyrometer(float gyroRates[3]) 
{
    gyroRates[0] = gyro.X;
    gyroRates[1] = gyro.Y;
    gyroRates[2] = gyro.Z;

    return true;
}

void AHackflightSimPawn::writeMotor(uint8_t index, float value) 
{
    motorvals[index] = value;
}

uint8_t AHackflightSimPawn::serialAvailableBytes(void)
{ 
    /*
	if (server.connected()) {
		Debug::printf("Connected (%d FPS)", fps);
		static int count;
		char buf[80] = "";
		if (server.receiveBuffer(buf, 80) > 0) {
			hf::Debug::printf("Client said: %s", buf);
			sprintf_s(buf, "%d", count++);
			server.sendBuffer(buf, strlen(buf));
		}
	}
    */

    hf::Debug::printf("serialAvailableBytes");

    return 0; 
}

uint8_t AHackflightSimPawn::serialReadByte(void)
{ 
    return 0; 
}

void AHackflightSimPawn::serialWriteByte(uint8_t c)
{ 
    if (server.connected()) {
        server.sendBuffer((char *)&c, 1);
    }
}

