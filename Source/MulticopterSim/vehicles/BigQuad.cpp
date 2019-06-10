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
//
// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics,  "BigQuad/Frame.Frame",     FrameStatics)
DECLARE_STATIC_MESH(FMotorStatics,  "BigQuad/Motor.Motor",     MotorStatics)
DECLARE_STATIC_MESH(FProp1WStatics, "BigQuad/PropCCW.PropCCW", Prop1Statics)
DECLARE_STATIC_MESH(FProp2WStatics, "BigQuad/PropCCW.PropCCW", Prop2Statics)
DECLARE_STATIC_MESH(FProp3WStatics, "BigQuad/PropCW.PropCW",   Prop3Statics)
DECLARE_STATIC_MESH(FProp4WStatics, "BigQuad/PropCW.PropCW",   Prop4Statics)


// Constructor
ABigQuadPawn::ABigQuadPawn()
{
	// The Vehicle object will handle most of the work for the pawn
	_vehicle = new Vehicle(this, FrameStatics.mesh.Get(), new QuadXAPDynamics(&_params), 4);

    // Build the motor and propeller meshes specific to this vehicle

    float cx = -58.5, cy = +13, w = 35, h = 35, mo = +1, mz = 45, pz = 45;

    _vehicle->addMotor(0, -1, TEXT("Motor1Mesh"), MotorStatics.mesh.Get(), FVector(cx+w, cy+h+mo, mz), 
            TEXT("Prop1Mesh"), Prop1Statics.mesh.Get(), FVector(cx+w, cy+h, pz)); 

    _vehicle->addMotor(1, -1, TEXT("Motor2Mesh"), MotorStatics.mesh.Get(), FVector(cx-w, cy-h+mo, mz), 
            TEXT("Prop2Mesh"), Prop2Statics.mesh.Get(), FVector(cx-w, cy-h, pz)); 

    _vehicle->addMotor(2, +1, TEXT("Motor3Mesh"), MotorStatics.mesh.Get(), FVector(cx+w, cy-h+mo, mz),
            TEXT("Prop3Mesh"), Prop3Statics.mesh.Get(), FVector(cx+w, cy-h, pz)); 

    _vehicle->addMotor(3, +1, TEXT("Motor4Mesh"), MotorStatics.mesh.Get(), FVector(cx-w, cy+h+mo, mz),
            TEXT("Prop4Mesh"), Prop4Statics.mesh.Get(), FVector(cx-w, cy+h, pz)); 

    // Accessing camera render targets from map is done statically (at compile time).
    static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>
        cameraTextureObject(TEXT("/Game/Flying/RenderTargets/cameraRenderTarget"));

    // Get texture object from each render target
    _cameraRenderTarget = cameraTextureObject.Object;

    FVector cameraScale(0.1, 0.1, 0.1);

    _gimbalSpringArm = CreateDefaultSubobject<USpringArmComponent>(TEXT("gimbalSpringArm"));
    _gimbalSpringArm->SetupAttachment(RootComponent);
    _gimbalSpringArm->TargetArmLength = 0.f; // The camera follows at this distance behind the character

    _camera = CreateDefaultSubobject<UCameraComponent>(TEXT("camera"));
    _camera ->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName); 	
    _camera->SetWorldScale3D(cameraScale);
    _camera->SetFieldOfView(90);
    _camera->SetAspectRatio(4./3);

    _capture = CreateDefaultSubobject<USceneCaptureComponent2D >(TEXT("capture"));
    _capture->SetWorldScale3D(cameraScale);
    _capture->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName);
    _capture->TextureTarget = _cameraRenderTarget;
    _capture->FOVAngle = 45;

}

ABigQuadPawn::~ABigQuadPawn()
{
    delete _vehicle;
}

void ABigQuadPawn::PostInitializeComponents()
{
    _vehicle->PostInitializeComponents();

    Super::PostInitializeComponents();
}

// Called when the game starts or when spawned
void ABigQuadPawn::BeginPlay()
{
    videoManagerStart();

    _vehicle->BeginPlay();

    Super::BeginPlay();
}

void ABigQuadPawn::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    videoManagerStop();

    _vehicle->EndPlay();

    Super::EndPlay(EndPlayReason);
}

// Called automatically on main thread
void ABigQuadPawn::Tick(float DeltaSeconds)
{
    // Switch cameras periodically for testing
    switchCameras(DeltaSeconds);

    videoManagerGrabImage();

    _vehicle->Tick();

    Super::Tick(DeltaSeconds);
}

void ABigQuadPawn::setCameraFOV(float cameraFieldOfView, float captureFOVAngle)
{
    _camera->SetFieldOfView(cameraFieldOfView);
    _capture->FOVAngle = captureFOVAngle;
}

void ABigQuadPawn::switchCameras(float DeltaSeconds)
{
    static bool useWideAngle;
    static float time;
    static float prevTime;
    time += DeltaSeconds;
    if (time - prevTime > 2) {
        prevTime = time;
        useWideAngle = !useWideAngle;
        if (useWideAngle) {
            setCameraFOV(135, 90);
        }
        else {
            setCameraFOV(90, 45);
        }
    }
}

void ABigQuadPawn::setGimbal(void)
{
    // Get gimbal location from flight manager
    float roll = 0, pitch = 0;
    //_flightManager->getGimbal(roll, pitch);

    FRotator rotation = _gimbalSpringArm->GetComponentRotation();

    rotation.Roll  += roll;
    rotation.Pitch -= pitch;

    _gimbalSpringArm->SetWorldRotation(rotation);
}

#ifdef _USE_OPENCV

void ABigQuadPawn::videoManagerStart(void)
{
    _videoManager = FVideoManager::create(_cameraRenderTarget);
}

void ABigQuadPawn::videoManagerStop(void)
{
    _videoManager = (FVideoManager *)FThreadedWorker::stopThreadedWorker(_videoManager);
}

void ABigQuadPawn::videoManagerGrabImage(void)
{
    _videoManager->grabImage();
}

#else

void ABigQuadPawn::videoManagerStart(void) { }
void ABigQuadPawn::videoManagerStop(void) { }
void ABigQuadPawn::videoManagerGrabImage(void) { }

#endif



