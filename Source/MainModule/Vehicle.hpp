/*
 * Header-only support for vehicles in MulticopterSim
 *
 * This class peforms the following functions:
 *
 * (1) Statically builds meshes, cameras, and other UE4 objects
 *
 * (2) Provides basic support for displaying vehicle kinematics
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
#include "Camera.hpp"
#include "GimbalManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "GameFramework/SpringArmComponent.h"

#include <stdio.h>

// Windows/Linux compatibility 
#ifdef _WIN32
#define SPRINTF sprintf_s
#else
#include <wchar.h>
#define SPRINTF sprintf
#endif

// A macro for simplifying the declaration of static meshes
#define DECLARE_STATIC_MESH(structname, assetstr, objname)   \
    struct structname {                                             \
        ConstructorHelpers::FObjectFinderOptional<UStaticMesh> mesh;   \
        structname() : mesh(TEXT("/Game/Flying/Meshes/" assetstr)) { } \
    };                                                                     \
    static structname objname;

class Vehicle {

    private: 
        
        // Arbitrary array limits supporting statically declared assets
        static const uint8_t MAX_MOTORS  = 20; 
        static const uint8_t MAX_CAMERAS = 10; 

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

            Camera                   * cameras[MAX_CAMERAS];
            uint8_t                    cameraCount;

        } objects_t;

    private:

		static constexpr float CAMERA_X = +20;
		static constexpr float CAMERA_Y =   0;
		static constexpr float CAMERA_Z = +30;

		MultirotorDynamics * _dynamics = NULL;

        int8_t _motorDirections[MAX_MOTORS];

        // Threaded worker for running flight control
        class FFlightManager * _flightManager = NULL;

        // Bozo filter for failure to select a map
        bool _mapSelected = false;

        // Motor values for animation/sound
        float  _motorvals[MAX_MOTORS] = {};

        // Circular buffer for moving average of motor values
        TCircularBuffer<float> * _motorBuffer = NULL;
        uint32_t _bufferIndex = 0;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // A hack to avoid accessing kinematics before dynamics thread is ready
        uint32_t _count = 0;

        uint32_t _starts = 0;

        // Retrieves kinematics from dynamics computed in another thread
        bool getKinematics(void)
        {
			// FlightManager will be null after crash
			if (!_flightManager) return false;

            // Get current pose kinematics and motor values dynamics (from flight
            // manager). Motor values are used only for animation effects (prop
            // rotation, sound).
            FVector location;
            FRotator rotation;

            // Get vehicle pose from dynamics
            MultirotorDynamics::pose_t pose = {};
            _dynamics->getPose(pose);

            // Convert NED meters => ENU centimeters
            location.X =  pose.location[0] * 100; 
            location.Y =  pose.location[1] * 100; 
            location.Z = -pose.location[2] * 100; 

            // Convert radians to degrees
            rotation.Roll =  FMath::RadiansToDegrees(pose.rotation[0]);
            rotation.Pitch = FMath::RadiansToDegrees(pose.rotation[1]);
            rotation.Yaw =   FMath::RadiansToDegrees(pose.rotation[2]);

            if (_dynamics->crashed()) return false;

            _objects.pawn->SetActorLocation(location);
            _objects.pawn->SetActorRotation(rotation);

            return true;
        }

        // Animation effects (sound, spinning props)

        void addAnimationEffects(void)
        {
            // Get motor values from dynamics
            _flightManager->getMotorValues(_motorvals);

            // Compute the sum of the motor values
            float motorsum = 0;
            for (uint8_t j=0; j<_dynamics->motorCount(); ++j) {
                motorsum += _motorvals[j];
            }

            // Rotate props. For visual effect, we can ignore actual motor values, and just keep increasing the rotation.
            if (motorsum > 0) {
                rotateProps(_motorDirections, _dynamics->motorCount());
            }

            // Add mean to circular buffer for moving average
            _bufferIndex = _motorBuffer->GetNextIndex(_bufferIndex);
            (*_motorBuffer)[_bufferIndex] = motorsum / _dynamics->motorCount();

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

        void reportStatus(void)
        {
            // Un-comment this to track the number of (re)starts
            //debug("starts: %d", _starts);

            // Report FPS
            if (_flightManager) {

                // Get a high-fidelity current time value from the OS
                double currentTime = FPlatformTime::Seconds() - _startTime;
                //debug("FPS:  Main=%d    Flight=%d", (int)(++_count/currentTime), (int)(_flightManager->getCount()/currentTime));
            }
        }

        void grabImages(void)
        {
            for (uint8_t i=0; i<_objects.cameraCount; ++i) {
                _objects.cameras[i]->grabImage();
            }
        }

    public:

        void build(APawn * pawn, UStaticMesh * frameMesh)
        {
            _objects.pawn = pawn;
            _objects.frameMesh = frameMesh;

            _objects.frameMeshComponent = _objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(TEXT("FrameMesh"));
            _objects.frameMeshComponent->SetStaticMesh(_objects.frameMesh);
            _objects.pawn->SetRootComponent(_objects.frameMeshComponent);

            // Turn off UE4 physics
            _objects.frameMeshComponent->SetSimulatePhysics(false);

            // Get sound cue from Contents
            static ConstructorHelpers::FObjectFinder<USoundCue> soundCue(TEXT("/Game/Flying/Audio/MotorSoundCue"));

            // Store a reference to the Cue asset - we'll need it later.
            _objects.soundCue = soundCue.Object;

            // Create an audio component, the audio component wraps the Cue, 
            // and allows us to ineract with it, and its parameters from code.
            _objects.audioComponent = _objects.pawn->CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

            // Stop the sound from sound playing the moment it's created.
            _objects.audioComponent->bAutoActivate = false;

            // Attach the sound to the pawn's root, the sound follows the pawn around
            _objects.audioComponent->SetupAttachment(_objects.pawn->GetRootComponent());

            // Create a spring-arm for the gimbal
            _objects.springArm = _objects.pawn->CreateDefaultSubobject<USpringArmComponent>(TEXT("SpringArm"));
            _objects.springArm->SetupAttachment(_objects.pawn->GetRootComponent());
            _objects.springArm->TargetArmLength = 0.f; 
          }

        void addMesh(UStaticMesh * mesh, const char * name, const FVector & location, const FRotator rotation, const FVector & scale)
        {
            UStaticMeshComponent * meshComponent = 
                _objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(FName(name));
            meshComponent->SetStaticMesh(mesh);
            meshComponent->SetupAttachment(_objects.frameMeshComponent, USpringArmComponent::SocketName); 	
            meshComponent->AddRelativeLocation(location*100); // m => cm
            meshComponent->AddLocalRotation(rotation);
			meshComponent->SetRelativeScale3D(scale);
        }

        void addProp(uint8_t index, float x, float y, const float z, UStaticMesh * propMesh)
        {
            UStaticMeshComponent * pMeshComponent = 
                _objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Prop", index, "Mesh"));
            pMeshComponent->SetStaticMesh(propMesh);
            pMeshComponent->SetupAttachment(_objects.frameMeshComponent, USpringArmComponent::SocketName);
            pMeshComponent->AddRelativeLocation(FVector(x, y, z)*100); // m => cm

            _objects.propellerMeshComponents[index] = pMeshComponent;
        }

        void rotateProps(int8_t * motorDirections, uint8_t motorCount)
        {
            static float rotation;
            for (uint8_t i=0; i<motorCount; ++i) {
                _objects.propellerMeshComponents[i]->SetRelativeRotation(FRotator(0, rotation * motorDirections[i] * 100, 0));
            }
            rotation++;
        }

        void addCamera(Camera * camera)
        {
            // Use one-based indexing for asset names
            uint8_t id = _objects.cameraCount + 1;

            // Create name of render target asset
            wchar_t renderTargetName[200];
            swprintf(renderTargetName, sizeof(renderTargetName)/sizeof(*renderTargetName), 
                    L"/Game/Flying/RenderTargets/renderTarget_%dx%d_%d", camera->_cols, camera->_rows, id);  

            // Make the camera appear small in the editor so it doesn't obscure the vehicle
            FVector cameraScale(0.1, 0.1, 0.1);

            // Create a static render target.  This provides less flexibility than creating it dynamically,
            // but acquiring the pixels seems to run twice as fast.
            static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>cameraTextureObject((TCHAR *)renderTargetName);
            UTextureRenderTarget2D * textureRenderTarget2D = cameraTextureObject.Object;

            // Create a camera component 
            camera->_cameraComponent = _objects.pawn->CreateDefaultSubobject<UCameraComponent>(makeName("Camera", id));
            camera->_cameraComponent->SetupAttachment(_objects.springArm, USpringArmComponent::SocketName); 	
            camera->_cameraComponent->SetRelativeLocation(FVector(CAMERA_X, CAMERA_Y, CAMERA_Z));
            camera->_cameraComponent->SetWorldScale3D(cameraScale);
            camera->_cameraComponent->SetFieldOfView(camera->_fov);
            camera->_cameraComponent->SetAspectRatio((float)camera->_cols / camera->_rows);

            // Create a scene-capture component and set its target to the render target
            camera->_captureComponent = _objects.pawn->CreateDefaultSubobject<USceneCaptureComponent2D >(makeName("Capture", id));
            camera->_captureComponent->SetWorldScale3D(cameraScale);
            camera->_captureComponent->SetupAttachment(_objects.springArm, USpringArmComponent::SocketName);
            camera->_captureComponent->SetRelativeLocation(FVector(CAMERA_X, CAMERA_Y, CAMERA_Z));
            camera->_captureComponent->FOVAngle = camera->_fov - 45;
            camera->_captureComponent->TextureTarget = textureRenderTarget2D;

            // Get the render target resource for copying the image pixels
            camera->_renderTarget = textureRenderTarget2D->GameThread_GetRenderTargetResource();

            // Increment the camera count for next time
            _objects.cameras[_objects.cameraCount++] = camera;
        }

        static const FName makeName(const char * prefix, const uint8_t index, const char * suffix="")
        {
            char name[200];
            SPRINTF(name, "%s%d%s", prefix, index+1, suffix);
            return FName(name);
        }

        // Constructors
        
        Vehicle(void)
        {
            _dynamics = NULL;
            _flightManager = NULL;
         }

        Vehicle(MultirotorDynamics * dynamics)
        {
            _dynamics = dynamics;

            for (uint8_t i=0; i<dynamics->motorCount(); ++i) {
                _motorDirections[i] = dynamics->motorDirection(i);
            }

            _flightManager = NULL;
         }
        
        Vehicle(const objects_t & objects, MultirotorDynamics * dynamics)
        {
            _dynamics = dynamics;

            _objects.pawn               = objects.pawn;
            _objects.frameMesh          = objects.frameMesh;
            _objects.motorMesh          = objects.motorMesh;
            _objects.frameMeshComponent = objects.frameMeshComponent;
            _objects.soundCue           = objects.soundCue;
            _objects.audioComponent     = objects.audioComponent;

            _objects.springArm          = objects.springArm;

            for (uint8_t i=0; i<objects.cameraCount; ++i) {
                _objects.cameras[i] = objects.cameras[i];
            }
            _objects.cameraCount = objects.cameraCount;


            for (uint8_t i=0; i<dynamics->motorCount(); ++i) {
                _objects.propellerMeshComponents[i] = objects.propellerMeshComponents[i]; 
                _motorDirections[i] = dynamics->motorDirection(i);
            }

            _flightManager = NULL;
        }        

        ~Vehicle(void) 
        {
        }

        void BeginPlay(FFlightManager * flightManager)
        {
            _flightManager = flightManager;

            // Make sure a map has been selected
            FString mapName = _objects.pawn->GetWorld()->GetMapName();
            _mapSelected = !mapName.Contains("Untitled");

            _starts = 0;

            if (_mapSelected) {

                // Reset FPS count
                _startTime = FPlatformTime::Seconds();
                _count = 0;

                // Start the audio for the propellers Note that because the
                // Cue Asset is set to loop the sound, once we start playing the sound, it
                // will play continiously...
                //_objects.audioComponent->Play();
                debug("%p", _objects.audioComponent);

                // Create circular queue for moving-average of motor values
                _motorBuffer = new TCircularBuffer<float>(20);

                // Get vehicle ground-truth location and rotation to initialize flight manager, now and after any crashes
                FVector  startLocation = _objects.pawn->GetActorLocation();
                FRotator startRotation = _objects.pawn->GetActorRotation(); 
                MultirotorDynamics::pose_t pose = {};

                // Convert ENU centimeters => NED meters
                pose.location[0] =  startLocation.X / 100;
                pose.location[1] =  startLocation.Y / 100;
                pose.location[2] = -startLocation.Z / 100;

                // Convert degrees => radians
                pose.rotation[0] = FMath::DegreesToRadians(startRotation.Roll);
                pose.rotation[1] = FMath::DegreesToRadians(startRotation.Pitch);
                pose.rotation[2] = FMath::DegreesToRadians(startRotation.Yaw);

                // Initialize dynamics with initial pose
                _dynamics->init(pose);
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
                if (getKinematics()) {

                    // Keepin' it real(istic)!
                    addAnimationEffects();

                    // Grab images
                    grabImages();

                    // Report status (optional)
                    //reportStatus();
                }
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

        // Called on main thread
        void setGimbal(FGimbalManager * gimbalManager)
        {
            // FlightManager will be null after crash
            if (!_flightManager) return;

            // Get gimbal info from manager
            float roll = 0, pitch = 0, yaw = 0, fov = 0;
            gimbalManager->get(roll, pitch, yaw, fov);

            FRotator rotation = _objects.springArm->GetComponentRotation();

            rotation.Roll  += roll;
            rotation.Pitch -= pitch;
            rotation.Yaw   += yaw;

            _objects.springArm->SetWorldRotation(rotation);

            // XXX should we enable setting each camera's FOV independently?
            for (uint8_t i=0; i<_objects.cameraCount; ++i) {
                _objects.cameras[i]->_cameraComponent->FieldOfView = fov;
                _objects.cameras[i]->_captureComponent->FOVAngle = fov - 45;
            }
        }

    private:

        objects_t _objects;

}; // class Vehicle
