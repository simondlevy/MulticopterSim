/*
 * Header-only support for vehicles in MulticopterSim
 *
 * This class peforms two functions:
 *
 * (1) Provides basic support for displaying vehicle kinematics
 *
 * (2) Sub-classes the MultirotorDynamics class so that a Vehicle
 *     subclass can compute the U values for a particualr frame
 *     (e.g., QuadXAP)
 * 
 * Copyright (C) 2019 Simon D. Levy, Daniel Katzav
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include "Debug.hpp"

#include "dynamics/MultirotorDynamics.hpp"

#include "FlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "GameFramework/SpringArmComponent.h"
#include "Runtime/Engine/Classes/Kismet/KismetRenderingLibrary.h"

#ifdef _USE_OPENCV
#include "VideoManager.hpp"
#endif

#include <stdio.h>

// Windows/Linux compatibility 
#ifdef _WIN32
#define SPRINTF sprintf_s
#else
#define SPRINTF sprintf
#endif

// A macro for simplifying the declaration of static meshes
#define DECLARE_STATIC_MESH(structname, assetstr, objname)   \
    struct structname {                                             \
        ConstructorHelpers::FObjectFinderOptional<UStaticMesh> mesh;   \
        structname() : mesh(TEXT("/Game/Flying/Meshes/" assetstr)) { } \
    };                                                                     \
    static structname objname;

class MULTICOPTERSIM_API Vehicle : public MultirotorDynamics {

    private:

		static constexpr float CAMERA_Z = 25;

        static const uint8_t MAX_MOTORS = 100; // silly but simple

        uint8_t _motorCount = 0;

        // Threaded workers for running flight control, video
        class FFlightManager * _flightManager = NULL;

        // Bozo filter for failure to select a map
        bool _mapSelected = false;

        // Motor values for animation/sound
        float  _motorvals[MAX_MOTORS] = {0};

        // Circular buffer for moving average of motor values
        TCircularBuffer<float> * _motorBuffer = NULL;
        uint32_t _bufferIndex = 0;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // A hack to avoid accessing kinematics before dynamics thread is ready
        uint32_t _count = 0;

#ifdef _USE_OPENCV
        // Threaded worker for managing video from camera
        class FVideoManager * _videoManager = NULL;
        class FVideoManager * _videoManager2 = NULL;

        void videoManagerStart(void)
        {
            extern FVideoManager * createVideoManager(UTextureRenderTarget2D * cameraRenderTarget, short port);
            _videoManager = createVideoManager(_objects.renderTarget, 5000);
            _videoManager2 = createVideoManager(_objects.renderTarget2, 5001);
        }

        void videoManagerStop(void)
        {
            delete _videoManager;
            _videoManager = NULL;
            delete _videoManager2;
            _videoManager2 = NULL;
        }

        void videoManagerGrabImage(void)
        {
            _videoManager->grabImage();
            _videoManager2->grabImage();
        }
#else
        void videoManagerStart(void) { }
        void videoManagerStop(void) { }
        void videoManagerGrabImage(void) { }
#endif

        // Retrieves kinematics from dynamics computed in another thread
        void getKinematics(void)
        {
            // Get current pose kinematics and motor values dynamics (from flight
            // manager). Motor values are used only for animation effects (prop
            // rotation, sound).
            FVector location;
            FRotator rotation;

            bool crashed = _flightManager->getKinematics(location, rotation, _motorvals);

            if (crashed) {

                // Restart flight manager and video
                stopManagers();
            }

            _objects.pawn->SetActorLocation(location);
            _objects.pawn->SetActorRotation(rotation);
        }

        // Animation effects (sound, spinning props)

        void addAnimationEffects(void)
        {
            // Compute the sum of the motor values
            float motorsum = 0;
            for (uint8_t j=0; j<_motorCount; ++j) {
                motorsum += _motorvals[j];
            }

            // Rotate props. For visual effect, we can ignore actual motor values, and just keep increasing the rotation.
            if (motorsum > 0) {
                static float rotation;
                for (uint8_t i=0; i<_motorCount; ++i) {
                    _objects.propellerMeshComponents[i]->SetRelativeRotation(FRotator(0, rotation * motorDirection(i) * 100, 0));
                }
                rotation++;
            }

            // Add mean to circular buffer for moving average
            _bufferIndex = _motorBuffer->GetNextIndex(_bufferIndex);
            (*_motorBuffer)[_bufferIndex] = motorsum / _motorCount;

            // Compute the mean motor value over the buffer frames
            float smoothedMotorMean = 0;
            for (uint8_t i=0; i<_motorBuffer->Capacity(); ++i) {
                smoothedMotorMean += (*_motorBuffer)[i];
            }
            smoothedMotorMean /= _motorBuffer->Capacity();

            // Use the mean motor value to modulate the pitch and voume of the propeller sound
            _objects.audioComponent->SetFloatParameter(FName("pitch"), smoothedMotorMean);
            _objects.audioComponent->SetFloatParameter(FName("volume"), smoothedMotorMean);
        }

        // Starting pose for reset on crash
        FVector _startLocation;
        FRotator _startRotation;

        // Flight management thread
        void startFlightManagers(void)
        {
            extern FFlightManager * createFlightManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation);
            _flightManager = createFlightManager(this, _startLocation, _startRotation);
            videoManagerStart();
        }        

        void stopManagers(void)
        {
            delete _flightManager;
            videoManagerStop();
        }

        static const FName makeName(const char * prefix, const uint8_t index, const char * suffix)
        {
            char name[100];
            SPRINTF(name, "%s%d%s", prefix, index+1, suffix);
            return FName(name);
        }

    public:

        // Frame constants
        typedef struct {

            float cx;   // center X
            float cy;   // center Y
            float mo;   // motor offset
            float wd;   // width
            float ln;   // length
            float mz;   // motor Z
            float pz;   // propeller Z

        } layout_t;


        // UE4 objects that must be built statically
        typedef struct {

            APawn                    * pawn;
            UStaticMesh              * frameMesh;
            UStaticMesh              * motorMesh;
            UStaticMeshComponent     * frameMeshComponent;
            UStaticMeshComponent     * propellerMeshComponents[MAX_MOTORS];
            USoundCue                * soundCue;
            UAudioComponent          * audioComponent;

            USpringArmComponent      * springArm;
            UCameraComponent         * camera;
            USceneCaptureComponent2D * capture;
            UTextureRenderTarget2D   * renderTarget;

            UCameraComponent         * camera2;
            USceneCaptureComponent2D * capture2;
            UTextureRenderTarget2D   * renderTarget2;

            UTextureRenderTarget2D   * renderTarget3;

        } objects_t;

        // Constructor
        Vehicle(const objects_t & objects, const params_t & params, uint8_t motorCount)
            : MultirotorDynamics(params, motorCount)
        {
            _motorCount = motorCount;

            _objects.pawn               = objects.pawn;
            _objects.frameMesh          = objects.frameMesh;
            _objects.motorMesh          = objects.motorMesh;
            _objects.frameMeshComponent = objects.frameMeshComponent;
            _objects.soundCue           = objects.soundCue;
            _objects.audioComponent     = objects.audioComponent;

            _objects.springArm          = objects.springArm;
            _objects.camera             = objects.camera;
            _objects.capture            = objects.capture;
            _objects.renderTarget       = objects.renderTarget;

            _objects.capture2           = objects.capture2;
            _objects.camera2            = objects.camera2;
            _objects.renderTarget2      = objects.renderTarget2;

            _objects.renderTarget3      = objects.renderTarget3;

            for (uint8_t i=0; i<motorCount; ++i) {
                _objects.propellerMeshComponents[i] = objects.propellerMeshComponents[i]; 
            }
        }        

        ~Vehicle(void) 
        {
        }

        void BeginPlay()
        {
            // Make sure a map has been selected
            FString mapName = _objects.pawn->GetWorld()->GetMapName();
            _mapSelected = !mapName.Contains("Untitled");

            if (_mapSelected) {


                // Reset FPS count
                _startTime = FPlatformTime::Seconds();
                _count = 0;

                // Start the audio for the propellers Note that because the
                // Cue Asset is set to loop the sound, once we start playing the sound, it
                // will play continiously...
                _objects.audioComponent->Play();

                // Get vehicle ground-truth location and rotation to initialize flight manager, now and after any crashes
                _startLocation = _objects.pawn->GetActorLocation();
                _startRotation = _objects.pawn->GetActorRotation(); 

                // Create circular queue for moving-average of motor values
                _motorBuffer = new TCircularBuffer<float>(20);

                // Initialize threaded workers
                startFlightManagers();
            }

            else {
                error("NO MAP SELECTED");
            }
        }

        void Tick(void)
        {
            // Checking count is a hack to avoid accessing kinematics before dynamics thread is ready
            if (_mapSelected && _count++>10) {

                // Kinematics from dynamics
                getKinematics();

                // Keepin' it real(istic)!
                addAnimationEffects();

                // Move gimbal and get Field-Of-View
                setGimbal();

                // Grab image
                videoManagerGrabImage();

                // Get a high-fidelity current time value from the OS
                double currentTime = FPlatformTime::Seconds() - _startTime;

                // OSD for debugging messages from threaded workers
                debug("Main:  FPS=%d    Flight: %s", (int)(++_count/currentTime), _flightManager->getMessage());
            }
        }

        void PostInitializeComponents()
        {
            // Add "Vehicle" tag for use by level blueprint
            _objects.pawn->Tags.Add(FName("Vehicle"));

            if (_objects.soundCue->IsValidLowLevelFast()) {
                _objects.audioComponent->SetSound(_objects.soundCue);
            }
        }

        void EndPlay(void)
        {
            if (_mapSelected) {

                stopManagers();
            }
        }

        void setGimbal(void)
        {
            // Get gimbal location from flight manager
            float roll = 0, pitch = 0, fov = 0;
            _flightManager->getGimbal(roll, pitch, fov);

            FRotator rotation = _objects.springArm->GetComponentRotation();

            rotation.Roll  += roll;
            rotation.Pitch -= pitch;

            _objects.springArm->SetWorldRotation(rotation);

            _objects.camera->FieldOfView = fov;
            _objects.capture->FOVAngle = fov - 45;

            _objects.camera2->FieldOfView = fov;
            _objects.capture2->FOVAngle = fov - 45;
        }

    protected:

        static void build(objects_t & objects)
        {
            objects.frameMeshComponent = objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(TEXT("FrameMesh"));
            objects.frameMeshComponent->SetStaticMesh(objects.frameMesh);
            objects.pawn->SetRootComponent(objects.frameMeshComponent);

            // Turn off UE4 physics
            objects.frameMeshComponent->SetSimulatePhysics(false);

            // Get sound cue from Contents
            static ConstructorHelpers::FObjectFinder<USoundCue> soundCue(TEXT("'/Game/Flying/Audio/MotorSoundCue'"));

            // Store a reference to the Cue asset - we'll need it later.
            objects.soundCue = soundCue.Object;

            // Create an audio component, the audio component wraps the Cue, 
            // and allows us to ineract with it, and its parameters from code.
            objects.audioComponent = objects.pawn->CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

            // Stop the sound from sound playing the moment it's created.
            objects.audioComponent->bAutoActivate = false;

            // Attach the sound to the pawn's root, the sound follows the pawn around
            objects.audioComponent->SetupAttachment(objects.pawn->GetRootComponent());

            // Get camera render target from Contents
            static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>
                cameraTextureObject(TEXT("/Game/Flying/RenderTargets/cameraRenderTarget"));

            static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>
                cameraTextureObject2(TEXT("/Game/Flying/RenderTargets/cameraRenderTarget_2"));

            // Get texture object from each render target
            objects.renderTarget = cameraTextureObject.Object;
            objects.renderTarget2 = cameraTextureObject2.Object;

            // Make the camera appear small in the editor so it doesn't obscure the vehicle
            FVector cameraScale(0.1, 0.1, 0.1);

            // Create a spring-arm for the gimbal
            objects.springArm = objects.pawn->CreateDefaultSubobject<USpringArmComponent>(TEXT("SpringArm"));
            objects.springArm->SetupAttachment(objects.pawn->GetRootComponent());
            objects.springArm->TargetArmLength = 0.f; 

            // Create a camera component 
            objects.camera = objects.pawn->CreateDefaultSubobject<UCameraComponent>(TEXT("Camera"));
            objects.camera ->SetupAttachment(objects.springArm, USpringArmComponent::SocketName); 	
			objects.camera->SetRelativeLocation(FVector(0, 0, CAMERA_Z));
            objects.camera->SetWorldScale3D(cameraScale);
            objects.camera->SetFieldOfView(90);
            objects.camera->SetAspectRatio(4./3);

            // Create a scene-capture component and set its target to the render target
            objects.capture = objects.pawn->CreateDefaultSubobject<USceneCaptureComponent2D >(TEXT("Capture"));
            objects.capture->SetWorldScale3D(cameraScale);
            objects.capture->SetupAttachment(objects.springArm, USpringArmComponent::SocketName);
			objects.capture->SetRelativeLocation(FVector(0, 0, CAMERA_Z));
            objects.capture->TextureTarget = objects.renderTarget;
            objects.capture->FOVAngle = 45;
            
            // Create a camera component 
            objects.camera2 = objects.pawn->CreateDefaultSubobject<UCameraComponent>(TEXT("Camera2"));
            objects.camera2 ->SetupAttachment(objects.springArm, USpringArmComponent::SocketName); 	
			objects.camera2->SetRelativeLocation(FVector(0, 0, CAMERA_Z));
            objects.camera2->SetWorldScale3D(cameraScale);
            objects.camera2->SetFieldOfView(90);
            objects.camera2->SetAspectRatio(4./3);

            // Create a scene-capture component and set its target to the render target
            objects.capture2 = objects.pawn->CreateDefaultSubobject<USceneCaptureComponent2D >(TEXT("Capture2"));
            objects.capture2->SetWorldScale3D(cameraScale);
            objects.capture2->SetupAttachment(objects.springArm, USpringArmComponent::SocketName);
			objects.capture2->SetRelativeLocation(FVector(0, 0, CAMERA_Z));
            objects.capture2->TextureTarget = objects.renderTarget;
            objects.capture2->FOVAngle = 45;

			//objects.renderTarget3 = NewObject<UTextureRenderTarget2D>(objects.pawn);
          }

        static void addMotor(objects_t & objects, uint8_t index, FVector mLocation, UStaticMesh * pMesh, FVector pLocation)
        {
            UStaticMeshComponent * mMeshComponent = 
                objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Motor", index, "Mesh"));
            mMeshComponent->SetStaticMesh(objects.motorMesh);
            mMeshComponent->SetupAttachment(objects.frameMeshComponent, USpringArmComponent::SocketName); 	
            mMeshComponent->AddRelativeLocation(mLocation*100); // m => cm

            UStaticMeshComponent * pMeshComponent = 
                objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Prop", index, "Mesh"));
            pMeshComponent->SetStaticMesh(pMesh);
            pMeshComponent->SetupAttachment(objects.frameMeshComponent, USpringArmComponent::SocketName);
            pMeshComponent->AddRelativeLocation(pLocation*100); // m => cm

            objects.propellerMeshComponents[index] = pMeshComponent;
        }

    private:

        objects_t _objects;

}; // class Vehicle
