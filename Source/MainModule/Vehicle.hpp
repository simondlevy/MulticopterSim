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

#include "Utils.hpp"
#include "dynamics/MultirotorDynamics.hpp"
#include "FlightManager.hpp"
#include "Camera.hpp"
#include "Landscape.h"

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
        
        // UE4 objects that must be built statically
        APawn                    * _pawn;
        UStaticMesh              * _frameMesh;
        UStaticMesh              * _motorMesh;
        UStaticMeshComponent     * _frameMeshComponent;
        UStaticMeshComponent     * _propellerMeshComponents[FFlightManager::MAX_MOTORS];
        USoundCue                * _soundCue;
        UAudioComponent          * _audioComponent;
        USpringArmComponent      * _springArm;
        uint8_t                    _propCount;

        // Cameras
        Camera * _cameras[Camera::MAX_CAMERAS];
        uint8_t  _cameraCount;

        MultirotorDynamics * _dynamics = NULL;

        int8_t _motorDirections[FFlightManager::MAX_MOTORS] = {};

        // Threaded worker for running flight control
        class FFlightManager * _flightManager = NULL;

        // Bozo filter for failure to select a map
        bool _mapSelected = false;

        // Motor values for animation/sound
        float  _motorvals[FFlightManager::MAX_MOTORS] = {};

        // Circular buffer for moving average of motor values
        TCircularBuffer<float> * _motorBuffer = NULL;
        uint32_t _bufferIndex = 0;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // A hack to avoid accessing kinematics before dynamics thread is ready
        uint32_t _count = 0;

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

            _pawn->SetActorLocation(location);
            _pawn->SetActorRotation(rotation);

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
            _audioComponent->SetFloatParameter(FName("pitch"), smoothedMotorMean);
            _audioComponent->SetFloatParameter(FName("volume"), smoothedMotorMean);
        }

        void grabImages(void)
        {
            for (uint8_t i=0; i<_cameraCount; ++i) {
                _cameras[i]->grabImage();
            }
        }

    public:

        void build(APawn * pawn, UStaticMesh * frameMesh)
        {
            _pawn = pawn;
            _frameMesh = frameMesh;

            _frameMeshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(TEXT("FrameMesh"));
            _frameMeshComponent->SetStaticMesh(_frameMesh);
            _pawn->SetRootComponent(_frameMeshComponent);

            // Turn off UE4 physics
            _frameMeshComponent->SetSimulatePhysics(false);

            _propCount = 0;
          }

        void buildWithAudio(APawn * pawn, UStaticMesh * frameMesh)
        {
            build(pawn, frameMesh);

            // Get sound cue from Contents
            static ConstructorHelpers::FObjectFinder<USoundCue> soundCue(TEXT("/Game/Flying/Audio/MotorSoundCue"));

            // Store a reference to the Cue asset - we'll need it later.
            _soundCue = soundCue.Object;

            // Create an audio component, which wraps the sound cue, and allows us to ineract with it and its parameters from code
            _audioComponent = _pawn->CreateDefaultSubobject<UAudioComponent>(TEXT("PropellerAudioComp"));

            // Set the audio component's volume to zero
            _audioComponent->SetFloatParameter(FName("volume"), 0);

            // Attach the sound to the pawn's root, the sound follows the pawn around
            _audioComponent->SetupAttachment(_pawn->GetRootComponent());

            // Create a spring-arm for the gimbal
            _springArm = _pawn->CreateDefaultSubobject<USpringArmComponent>(TEXT("SpringArm"));
            _springArm->SetupAttachment(_pawn->GetRootComponent());
            _springArm->TargetArmLength = 0.f; 
          }

        void addMesh(UStaticMesh * mesh, const char * name, const FVector & location, const FRotator rotation, const FVector & scale)
        {
            UStaticMeshComponent * meshComponent = 
                _pawn->CreateDefaultSubobject<UStaticMeshComponent>(FName(name));
            meshComponent->SetStaticMesh(mesh);
            meshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName); 	
            meshComponent->AddRelativeLocation(location*100); // m => cm
            meshComponent->AddLocalRotation(rotation);
			meshComponent->SetRelativeScale3D(scale);
        }

        void addMesh(UStaticMesh * mesh, const char * name, const FVector & location, const FRotator rotation, const float scale)
        {
            addMesh(mesh, name, location, rotation, FVector(1,1,1)*scale);
        }

        void addMesh(UStaticMesh * mesh, const char * name, const FVector & location, const FRotator rotation)
        {
            addMesh(mesh, name, location, rotation, 1.0);
        }

        void addMesh(UStaticMesh * mesh, const char * name, const FVector & location)
        {
            addMesh(mesh, name, location, FRotator(0,0,0), FVector(1,1,1));
        }

        void addMesh(UStaticMesh * mesh, const char * name, const FVector & location, const FVector & scale)
        {
            addMesh(mesh, name, location, FRotator(0,0,0), scale);
        }

        void addMesh(UStaticMesh * mesh, const char * name)
        {
            addMesh(mesh, name, FVector(0,0,0), FRotator(0,0,0), FVector(1,1,1));
        }

        void addProp(UStaticMesh * propMesh)
        {
            UStaticMeshComponent * pMeshComponent = 
                _pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Prop", _propCount, "Mesh"));
            pMeshComponent->SetStaticMesh(propMesh);
            pMeshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName);
            _propellerMeshComponents[_propCount++] = pMeshComponent;
        }

        void rotateProps(int8_t * motorDirections, uint8_t motorCount)
        {
            static float rotation;
            for (uint8_t i=0; i<motorCount; ++i) {
                _propellerMeshComponents[i]->SetRelativeRotation(FRotator(0, rotation * motorDirections[i] * 200, 0));
            }
            rotation++;
        }

        void addCamera(Camera * camera)
        {
            // Add camera to spring arm
            camera->addToVehicle(_pawn, _springArm, _cameraCount);

           // Increment the camera count for next time
            _cameras[_cameraCount++] = camera;
        }

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
        
        ~Vehicle(void) 
        {
        }

        void BeginPlay(FFlightManager * flightManager)
        {
            _flightManager = flightManager;

            // Make sure a map has been selected
            FString mapName = _pawn->GetWorld()->GetMapName();
            _mapSelected = !mapName.Contains("Untitled");

            // Bozo filter
            if (!_mapSelected) {
                error("NO MAP SELECTED");
                return;
            }

            // Reset FPS count
            _startTime = FPlatformTime::Seconds();
            _count = 0;

            // Start the audio for the propellers Note that because the
            // Cue Asset is set to loop the sound, once we start playing the sound, it
            // will play continiously...
            _audioComponent->Play();

            // Create circular queue for moving-average of motor values
            _motorBuffer = new TCircularBuffer<float>(20);

            // Get vehicle ground-truth location and rotation to initialize flight manager, now and after any crashes
            FVector  startLocation = _pawn->GetActorLocation();
            FRotator startRotation = _pawn->GetActorRotation(); 
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

        void Tick(void)
        {
            // Checking count is a hack to avoid accessing kinematics before dynamics thread is ready
            if (!_mapSelected || _count++<10 || !getKinematics()) return;

            // Keepin' it real(istic)!
            addAnimationEffects();

            // Grab images
            grabImages();

            // Compute AGL (height above ground level) and check it in dynamics
            _dynamics->checkAgl(computeAgl());
        }

        void PostInitializeComponents()
        {
            // Add "Vehicle" tag for use by level blueprint
            _pawn->Tags.Add(FName("Vehicle"));

            if (_soundCue->IsValidLowLevelFast()) {
                _audioComponent->SetSound(_soundCue);
            }
        }

        float computeAgl(void)
        {
            // See https://unrealcpp.com/line-trace-on-tick/

            // Start at a point at the bottom of the sphere enclosing the vehicle
			FVector startPoint = _pawn->GetActorLocation();
            startPoint.Z -= _frameMesh->GetBounds().GetSphere().W;

			// End at a point far below the sphere
            FVector endPoint = FVector(startPoint.X, startPoint.Y, startPoint.Z-1e6);

            //DrawDebugLine(_pawn->GetWorld(), startPoint, endPoint, FColor::Green, false, 1, 0, 0.5);

            // Trace a ray to the ground
            FHitResult OutHit;
            FCollisionQueryParams CollisionParams;
            if (_pawn->GetWorld()->LineTraceSingleByChannel(OutHit, startPoint, endPoint, ECC_Visibility, CollisionParams)) {
                if(OutHit.bBlockingHit && OutHit.GetActor()->GetName() == "Landscape_0") {
                    FVector impactPoint = OutHit.ImpactPoint;
					return (startPoint.Z - impactPoint.Z) / 100;
                }
            }

            // No AGL computed; return zero
            return 0;
        }

        void rotateGimbal(FQuat rotation)
        {
            _springArm->SetRelativeRotation(rotation);
        }

}; // class Vehicle
