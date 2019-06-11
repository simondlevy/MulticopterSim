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

#include "MultirotorDynamics.hpp"

#include "FlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "GameFramework/SpringArmComponent.h"

#ifdef _USE_OPENCV
#include "VideoManager.hpp"
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

        uint8_t _motorCount = 0;

        // Static mesh component for the vehicle frame
        class UStaticMeshComponent* _frameMeshComponent = NULL;

        // Static mesh components for the propellers
        class UStaticMeshComponent ** _propellerMeshComponents = NULL;

        // Spin directions for animating propellers
        int8_t * _propellerDirections = NULL;

        // Audio support: see http://bendemott.blogspot.com/2016/10/unreal-4-playing-sound-from-c-with.html
        class USoundCue* _propellerAudioCue;
        class USoundCue* _propellerStartupCue;
        class UAudioComponent* _propellerAudioComponent;

        // Threaded workers for running flight control, video
        class FFlightManager * _flightManager = NULL;

        // Bozo filter for failure to select a map
        bool _mapSelected = false;

        // Motor values for animation/sound
        float * _motorvals = NULL;

        // Support for camera gimbal
        class USpringArmComponent* _gimbalSpringArm;
        class UCameraComponent* _camera;
        class USceneCaptureComponent2D * _capture;

        // Render targets, passed to consgtructor for threaded video worker when Start button is pressed
        UTextureRenderTarget2D * _cameraRenderTarget;

#ifdef _USE_OPENCV
        // Threaded worker for managing video from camera
        class FVideoManager * _videoManager = NULL;

        void videoManagerStart(void)
        {
            _videoManager = FVideoManager::create(_cameraRenderTarget);
        }

        void videoManagerStop(void)
        {
            _videoManager = (FVideoManager *)FThreadedWorker::stopThreadedWorker(_videoManager);
        }

        void videoManagerGrabImage(void)
        {
            _videoManager->grabImage();
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

                // Restart threaded workers
                stopThreadedWorkers();
                startThreadedWorkers();
            }

            //debug("sx: %+3.3f sy: %+3.3f  | x: %+3.3f y: %+3.3f | crashed: %d", 
            //        _startLocation.X, _startLocation.Y, location.X, location.Y, crashed);

            _pawn->SetActorLocation(location);
            _pawn->SetActorRotation(rotation);
        }


        // Animation effects (sound, spinning props)

        void addAnimationEffects(void)
        {
            // Compute the mean of the motor values
            float motormean = 0;
            for (uint8_t j=0; j<_motorCount; ++j) {
                motormean += _motorvals[j];
            }
            motormean /= _motorCount;

            // Rotate props. For visual effect, we can ignore actual motor values, and just keep increasing the rotation.
            if (motormean > 0) {
                static float rotation;
                for (uint8_t i=0; i<_motorCount; ++i) {
                    _propellerMeshComponents[i]->SetRelativeRotation(FRotator(0, rotation * _propellerDirections[i] * 100, 0));
                }
                rotation++;
            }

            // Use the mean motor value to modulate the pitch and voume of the propeller sound
            setAudioPitchAndVolume(motormean);
        }

        void setAudioPitchAndVolume(float value)
        {
            _propellerAudioComponent->SetFloatParameter(FName("pitch"), value);
            _propellerAudioComponent->SetFloatParameter(FName("volume"), value);
        }

        // Starting pose for reset on crash
        FVector _startLocation;
        FRotator _startRotation;

        // Pawn
        APawn * _pawn = NULL;

        // Flight management thread
        void startThreadedWorkers(void)
        {
            _flightManager = FFlightManager::create(this, _startLocation, _startRotation);
            videoManagerStart();
        }        

        void stopThreadedWorkers(void)
        {
            _flightManager = (FFlightManager *)FThreadedWorker::stopThreadedWorker(_flightManager);
            videoManagerStop();
        }

    protected:

        void addMotor( uint8_t index, int8_t direction, const char * mMeshName, UStaticMesh * mMesh, FVector mLocation,
                const char * pMeshName, UStaticMesh * pMesh, FVector pLocation)
        {
            UStaticMeshComponent * mMeshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(FName(mMeshName));
            mMeshComponent->SetStaticMesh(mMesh);
            mMeshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName); 	
            mMeshComponent->AddRelativeLocation(mLocation*100); // m => cm

            UStaticMeshComponent * pMeshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(FName(pMeshName));
            pMeshComponent->SetStaticMesh(pMesh);
            pMeshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName);
            pMeshComponent->AddRelativeLocation(pLocation*100); // m => cm

            _propellerMeshComponents[index] = pMeshComponent;
            _propellerDirections[index] = direction;
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

        } frame_t;

        // Constructor
        Vehicle(APawn * pawn, UStaticMesh * frameMesh, const params_t * params, uint8_t motorCount)
            : MultirotorDynamics(params, motorCount)
        {
			_motorCount = motorCount;

            _propellerMeshComponents = new UStaticMeshComponent * [motorCount];

            _propellerDirections = new int8_t [motorCount];

            _frameMeshComponent = pawn->CreateDefaultSubobject<UStaticMeshComponent>(TEXT("FrameMesh"));
            _frameMeshComponent->SetStaticMesh(frameMesh);
            pawn->SetRootComponent(_frameMeshComponent);

            // Turn off UE4 physics
            _frameMeshComponent->SetSimulatePhysics(false);

            // Load our Sound Cue for the propeller sound we created in the editor... 
            // note your path may be different depending
            // on where you store the asset on disk.
            static ConstructorHelpers::FObjectFinder<USoundCue> propellerCue(TEXT("'/Game/Flying/Audio/MotorSoundCue'"));

            // Store a reference to the Cue asset - we'll need it later.
            _propellerAudioCue = propellerCue.Object;

            // Create an audio component, the audio component wraps the Cue, 
            // and allows us to ineract with it, and its parameters from code.
            _propellerAudioComponent = pawn->CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

            // Stop the sound from sound playing the moment it's created.
            _propellerAudioComponent->bAutoActivate = false;

            // Attach the sound to the pawn's root, the sound follows the pawn around
            _propellerAudioComponent->SetupAttachment(pawn->GetRootComponent());

            // Allocate space for motor values used in animation/sound
            _motorvals = new float[motorCount];

            // Store vehicle pawn and dynamics for later
            _pawn = pawn;


            // Accessing camera render targets from map is done statically (at compile time).
            static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>
                cameraTextureObject(TEXT("/Game/Flying/RenderTargets/cameraRenderTarget"));

            // Get texture object from each render target
            _cameraRenderTarget = cameraTextureObject.Object;

            FVector cameraScale(0.1, 0.1, 0.1);

            _gimbalSpringArm = pawn->CreateDefaultSubobject<USpringArmComponent>(TEXT("gimbalSpringArm"));
            _gimbalSpringArm->SetupAttachment(pawn->GetRootComponent());
            _gimbalSpringArm->TargetArmLength = 0.f; 

            _camera = pawn->CreateDefaultSubobject<UCameraComponent>(TEXT("camera"));
            _camera ->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName); 	
            _camera->SetWorldScale3D(cameraScale);
            _camera->SetFieldOfView(90);
            _camera->SetAspectRatio(4./3);

            _capture = pawn->CreateDefaultSubobject<USceneCaptureComponent2D >(TEXT("capture"));
            _capture->SetWorldScale3D(cameraScale);
            _capture->SetupAttachment(_gimbalSpringArm, USpringArmComponent::SocketName);
            _capture->TextureTarget = _cameraRenderTarget;
            _capture->FOVAngle = 45;
        }        

        ~Vehicle(void) 
        {
            delete _propellerMeshComponents;
            delete _propellerDirections;
            delete _motorvals;
        }

        void BeginPlay()
        {
            // Make sure a map has been selected
            FString mapName = _pawn->GetWorld()->GetMapName();
            _mapSelected = !mapName.Contains("Untitled");

            if (_mapSelected) {

                // Start the audio for the propellers Note that because the
                // Cue Asset is set to loop the sound, once we start playing the sound, it
                // will play continiously...
                _propellerAudioComponent->Play();

                // Get vehicle ground-truth location and rotation to initialize flight manager, now and after any crashes
                _startLocation = _pawn->GetActorLocation();
                _startRotation = _pawn->GetActorRotation(); 

                // Initialize threaded workers
                startThreadedWorkers();
            }

            else {
                error("NO MAP SELECTED");
            }
        }


        void Tick(void)
        {
            // A hack to avoid accessing kinematics before dynamics thread is ready
            static uint64_t count;

            if (_mapSelected && count++>10) {

                // Kinematics from dynamics
                getKinematics();

                // Keepin' it real(istic)!
                addAnimationEffects();

                // Move gimbal and get Field-Of-View
                setGimbal();

                // Grab image
                videoManagerGrabImage();

                // OSD for debugging messages from threaded workers
                //debug("%s", _flightManager->getMessage());
            }
        }

        void PostInitializeComponents()
        {
            // Add "Vehicle" tag for use by level blueprint
            _pawn->Tags.Add(FName("Vehicle"));

            if (_propellerAudioCue->IsValidLowLevelFast()) {
                _propellerAudioComponent->SetSound(_propellerAudioCue);
            }
        }

        void EndPlay(void)
        {
            if (_mapSelected) {

                stopThreadedWorkers();
            }
        }

        void setGimbal(void)
        {
            // Get gimbal location from flight manager
            float roll = 0, pitch = 0, fov = 0;
            _flightManager->getGimbal(roll, pitch, fov);

            FRotator rotation = _gimbalSpringArm->GetComponentRotation();

            rotation.Roll  += roll;
            rotation.Pitch -= pitch;

            _gimbalSpringArm->SetWorldRotation(rotation);

            _camera->FieldOfView = fov;
            _capture->FOVAngle = fov - 45;
        }

}; // class Vehicle
