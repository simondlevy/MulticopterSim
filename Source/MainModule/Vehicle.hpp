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
#include "VideoManager.hpp"
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
#define SWPRINTF swprintf_s
#else
#include <wchar.h>
#define SPRINTF sprintf
#define SWPRINTF swprintf
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
        static const uint8_t MAX_MOTORS = 20; 
        static const uint8_t MAX_CAMERAS = 10; 

        typedef struct {

            UCameraComponent         * cameraComponent;
            USceneCaptureComponent2D * captureComponent;
            UTextureRenderTarget2D   * renderTarget;
            VideoManager             * videoManager;

        } camera_t;

    public: 
        
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

            camera_t                   cameras[MAX_CAMERAS];

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
        float  _motorvals[MAX_MOTORS] = {0};

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
            MultirotorDynamics::state_t state = {0};
            _dynamics->getState(state);
            MultirotorDynamics::pose_t pose = state.pose;

            // Convert NED meters => ENU centimeters
            location.X =  pose.location[0] * 100; 
            location.Y =  pose.location[1] * 100; 
            location.Z = -pose.location[2] * 100; 

            // Convert radians to degrees
            rotation.Roll =  FMath::RadiansToDegrees(pose.rotation[0]);
            rotation.Pitch = FMath::RadiansToDegrees(pose.rotation[1]);
            rotation.Yaw =   FMath::RadiansToDegrees(pose.rotation[2]);

            // If we crashed, restart flight manager
            if (_dynamics->crashed()) {

                stopFlightManager();
                startFlightManager();

                return false;
            }

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
                rotateProps(_objects, _motorDirections, _dynamics->motorCount());
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


        // Flight management thread
        void startFlightManager(void)
        {
            _starts++;

            extern FFlightManager * createFlightManager(MultirotorDynamics * dynamics);

            _flightManager = createFlightManager(_dynamics);

        }        

        void stopFlightManager(void)
        {
            _flightManager = (FFlightManager *)FThreadedWorker::stopThreadedWorker(_flightManager);
        }

        void reportStatus(void)
        {
            // Get a high-fidelity current time value from the OS
            double currentTime = FPlatformTime::Seconds() - _startTime;

            // Un-comment this to track the number of (re)starts
            debug("starts: %d", _starts);

            // Report FPS
            if (_flightManager) {

                //debug("FPS:  Main=%d    Flight=%d", (int)(++_count/currentTime), (int)(_flightManager->getCount()/currentTime));
                //debug("%s", _flightManager->getMessage());
            }
        }

        void grabImages(void)
        {
            for (uint8_t i=0; i<_objects.cameraCount; ++i) {
                _objects.cameras[i].videoManager->grabImage();
            }
        }

        static void addCamera(objects_t & objects, float fov, const wchar_t * res)
        {
            // Use one-based indexing for asset names
            uint8_t id = objects.cameraCount + 1;

            // Create name of render target asset
            wchar_t renderTargetName[200];
            swprintf(renderTargetName, sizeof(renderTargetName)/sizeof(*renderTargetName), L"/Game/Flying/RenderTargets/renderTarget_%s_%d", res, id);  

            // Make the camera appear small in the editor so it doesn't obscure the vehicle
            FVector cameraScale(0.1, 0.1, 0.1);

            // Grab the current camera structure
            camera_t * cam = &objects.cameras[objects.cameraCount];

            // Create a static render target.  This provides less flexibility than creating it dynamically,
            // but acquiring the pixels seems to run twice as fast.
            static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>cameraTextureObject(renderTargetName);
            cam->renderTarget = cameraTextureObject.Object;

            // Create a camera component 
            cam->cameraComponent = objects.pawn->CreateDefaultSubobject<UCameraComponent>(makeName("Camera", id));
            cam->cameraComponent->SetupAttachment(objects.springArm, USpringArmComponent::SocketName); 	
            cam->cameraComponent->SetRelativeLocation(FVector(CAMERA_X, CAMERA_Y, CAMERA_Z));
            cam->cameraComponent->SetWorldScale3D(cameraScale);
            cam->cameraComponent->SetFieldOfView(fov);
            cam->cameraComponent->SetAspectRatio((float)cam->renderTarget->SizeX / cam->renderTarget->SizeY);

            // Create a scene-capture component and set its target to the render target
            cam->captureComponent = objects.pawn->CreateDefaultSubobject<USceneCaptureComponent2D >(makeName("Capture", id));
            cam->captureComponent->SetWorldScale3D(cameraScale);
            cam->captureComponent->SetupAttachment(objects.springArm, USpringArmComponent::SocketName);
            cam->captureComponent->SetRelativeLocation(FVector(CAMERA_X, CAMERA_Y, CAMERA_Z));
            cam->captureComponent->FOVAngle = fov - 45;
            cam->captureComponent->TextureTarget = cam->renderTarget;

            // Increment the camera count for next time
            objects.cameraCount++;
        }

    public:
        
        // Static helpers

        static void build(objects_t & objects)
        {
            objects.frameMeshComponent = objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(TEXT("FrameMesh"));
            objects.frameMeshComponent->SetStaticMesh(objects.frameMesh);
            objects.pawn->SetRootComponent(objects.frameMeshComponent);

            // Turn off UE4 physics
            objects.frameMeshComponent->SetSimulatePhysics(false);

            // Get sound cue from Contents
            static ConstructorHelpers::FObjectFinder<USoundCue> soundCue(TEXT("/Game/Flying/Audio/MotorSoundCue"));

            // Store a reference to the Cue asset - we'll need it later.
            objects.soundCue = soundCue.Object;

            // Create an audio component, the audio component wraps the Cue, 
            // and allows us to ineract with it, and its parameters from code.
            objects.audioComponent = objects.pawn->CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

            // Stop the sound from sound playing the moment it's created.
            objects.audioComponent->bAutoActivate = false;

            // Attach the sound to the pawn's root, the sound follows the pawn around
            objects.audioComponent->SetupAttachment(objects.pawn->GetRootComponent());

            // Create a spring-arm for the gimbal
            objects.springArm = objects.pawn->CreateDefaultSubobject<USpringArmComponent>(TEXT("SpringArm"));
            objects.springArm->SetupAttachment(objects.pawn->GetRootComponent());
            objects.springArm->TargetArmLength = 0.f; 
          }

        static void addMesh(const objects_t & objects, UStaticMesh * mesh, const char * name, 
                const FVector & location, const FRotator rotation, const FVector & scale)
        {

            UStaticMeshComponent * meshComponent = 
                objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(FName(name));
            meshComponent->SetStaticMesh(mesh);
            meshComponent->SetupAttachment(objects.frameMeshComponent, USpringArmComponent::SocketName); 	
            meshComponent->AddRelativeLocation(location*100); // m => cm
            meshComponent->AddLocalRotation(rotation);
			meshComponent->SetRelativeScale3D(scale);
        }

        static void addProp(objects_t & objects, uint8_t index, float x, float y, const float z, UStaticMesh * propMesh)
        {
            UStaticMeshComponent * pMeshComponent = 
                objects.pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Prop", index, "Mesh"));
            pMeshComponent->SetStaticMesh(propMesh);
            pMeshComponent->SetupAttachment(objects.frameMeshComponent, USpringArmComponent::SocketName);
            pMeshComponent->AddRelativeLocation(FVector(x, y, z)*100); // m => cm

            objects.propellerMeshComponents[index] = pMeshComponent;
        }

        static void rotateProps(objects_t & objects, int8_t * motorDirections, uint8_t motorCount)
        {
            static float rotation;
            for (uint8_t i=0; i<motorCount; ++i) {
                objects.propellerMeshComponents[i]->SetRelativeRotation(FRotator(0, rotation * motorDirections[i] * 100, 0));
            }
            rotation++;
        }

        static const FName makeName(const char * prefix, const uint8_t index, const char * suffix="")
        {
            char name[200];
            SPRINTF(name, "%s%d%s", prefix, index+1, suffix);
            return FName(name);
        }

        // Constructor
        Vehicle(const objects_t & objects, MultirotorDynamics * dynamics, FFlightManager * flightManager)
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
                _objects.cameras[i].cameraComponent = objects.cameras[i].cameraComponent;
                _objects.cameras[i].captureComponent = objects.cameras[i].captureComponent;
                _objects.cameras[i].renderTarget = objects.cameras[i].renderTarget;
                _objects.cameras[i].videoManager = NULL;
            }

            _objects.cameraCount = objects.cameraCount;


            for (uint8_t i=0; i<dynamics->motorCount(); ++i) {
                _objects.propellerMeshComponents[i] = objects.propellerMeshComponents[i]; 
                _motorDirections[i] = dynamics->motorDirection(i);
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

            _starts = 0;

            if (_mapSelected) {

                // Reset FPS count
                _startTime = FPlatformTime::Seconds();
                _count = 0;

                // Start the audio for the propellers Note that because the
                // Cue Asset is set to loop the sound, once we start playing the sound, it
                // will play continiously...
                _objects.audioComponent->Play();

                // Create circular queue for moving-average of motor values
                _motorBuffer = new TCircularBuffer<float>(20);

                // Create video manager(s)
                extern VideoManager * createVideoManager(UTextureRenderTarget2D * renderTarget, uint8_t id);
                for (uint8_t i=0; i<_objects.cameraCount; ++i) {

                    camera_t * cam = &_objects.cameras[i];
                    UTextureRenderTarget2D * renderTarget = cam->renderTarget;
                    cam->videoManager = createVideoManager(renderTarget, i);
                }

                // Initialize threaded workers
                startFlightManager();

                // Get vehicle ground-truth location and rotation to initialize flight manager, now and after any crashes
                FVector  startLocation = _objects.pawn->GetActorLocation();
                FRotator startRotation = _objects.pawn->GetActorRotation(); 
                MultirotorDynamics::pose_t pose = {0};

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

        void EndPlay(void)
        {
            if (_mapSelected) {

                stopFlightManager();

                // Free video managers
                for (uint8_t i=0; i<_objects.cameraCount; ++i) {
                    delete _objects.cameras[i].videoManager;
                }

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
                _objects.cameras[i].cameraComponent->FieldOfView = fov;
                _objects.cameras[i].captureComponent->FOVAngle = fov - 45;
            }
        }

        static void addCamera640x480(objects_t & objects, float fov)
        {
            addCamera(objects, fov, L"640x480");
        }

        static void addCamera1280x720(objects_t & objects, float fov)
        {
            addCamera(objects, fov, L"1280x720");
        }

        static void addCamera1920x1080(objects_t & objects, float fov)
        {
            addCamera(objects, fov, L"1920x1080");
        }

    private:

        objects_t _objects;

}; // class Vehicle
