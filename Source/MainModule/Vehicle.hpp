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

        // States
        typedef enum {

            STATE_NOMAP,
            STATE_CRASHED,
            STATE_READY,
            STATE_RUNNING,
            STATE_COUNT

        } kinematicState_t;

        kinematicState_t _kinematicState = STATE_NOMAP;
 
        // Motor values for animation/sound
        float  _motorvals[FFlightManager::MAX_MOTORS] = {};

        // Circular buffer for moving average of motor values
        TCircularBuffer<float> * _motorBuffer = NULL;
        uint32_t _bufferIndex = 0;

        // For computing AGL
        float _vehicleBottom = 0;

        // Useful constant for tracing rays
        static constexpr float INF = 1e6;

        // Retrieves kinematics from dynamics computed in another thread, returning true if vehicle is airborne, false otherwise.
        void updateKinematics(void)
        {
             // Get vehicle pose from dynamics
            MultirotorDynamics::pose_t pose = _dynamics->getPose();

            // Convert pose to UE4 location, rotator
            FVector location = FVector(pose.location[0], pose.location[1], -pose.location[2]) * 100;  // NED => ENU
            FRotator rotation = FMath::RadiansToDegrees(FRotator(pose.rotation[1], pose.rotation[2], pose.rotation[0]));

            // Set vehicle pose in animation
            _pawn->SetActorLocation(location);
            _pawn->SetActorRotation(rotation);

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
        
            for (uint8_t i=0; i<_cameraCount; ++i) {
                _cameras[i]->grabImage();
            }

        } // updateKinematics

    public:

        void build(APawn * pawn, UStaticMesh * frameMesh)
        {
            _pawn = pawn;
            _frameMesh = frameMesh;

            _frameMeshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(TEXT("FrameMesh"));
            _frameMeshComponent->SetStaticMesh(_frameMesh);
            _pawn->SetRootComponent(_frameMeshComponent);

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

            _kinematicState = STATE_NOMAP;

            // Make sure a map has been selected
            if (_pawn->GetWorld()->GetMapName().Contains("Untitled")) {
                error("NO MAP SELECTED");
                return;
            }

            // Rady to fly
            _kinematicState = STATE_READY;

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

        void Tick(float DeltaSeconds)
        {
            static bool reset;
            const char * states[STATE_COUNT] = {"NOMAP", "CRASHED", "READY", "RUNNING"};
            if (agl() == INF) {
                debugline("State: %s  AGL: n/a  reset: %d", states[_kinematicState], reset);
            }
            else {
                debugline("State: %s  AGL: %+3.2f   reset: %d", states[_kinematicState], agl(), reset);
            }

            switch (_kinematicState) {

                case STATE_NOMAP:
                case STATE_CRASHED: 
                    break;

                case STATE_READY:
                    if (agl() > 0) {
                        _kinematicState = STATE_RUNNING;
                        _frameMeshComponent->SetSimulatePhysics(false);
                    }
                    updateKinematics();
                    break;

                case STATE_RUNNING:
                    if (agl() <= 0) {
                        _dynamics->reset();
                        reset = true;
                    }
                    else {
                        reset = false;
                    }
                    updateKinematics();
                    break;

            } 
        }

        // Returns AGL when vehicle is level above ground, "infinity" otherwise
        float agl(void)
        {
            const float Z_OFFSET = 2.5;

            // Start at a point slightly below the bottom of the box enclosing the vehicle
            FVector startPoint = _pawn->GetActorLocation();
            startPoint.Z = startPoint.Z + _vehicleBottom - Z_OFFSET;

            // End at a point an "infinite" distance from the bottom
            FVector endPoint = FVector(startPoint.X, startPoint.Y, startPoint.Z-INF);

            drawHorizontal(startPoint);
            drawLine(startPoint, endPoint);

            return getImpactDistance(startPoint, endPoint, "Landscape_0");
        }

        float getImpactDistance(FVector startPoint, FVector endPoint, const char * meshName)
        {
            FHitResult OutHit;
            FCollisionQueryParams CollisionParams;
            if (_pawn->GetWorld()->LineTraceSingleByChannel(OutHit, startPoint, endPoint, ECC_Visibility, CollisionParams)) {
                if (OutHit.bBlockingHit && OutHit.GetActor()->GetName() == meshName) {
                    FVector impactPoint = OutHit.ImpactPoint;
                    return (startPoint.Z - impactPoint.Z) / 100;
                }
            }

            return INF;
        }

        void drawHorizontal(FVector point)
        {
            FVector lftPoint = FVector(point.X, point.Y-100, point.Z);
            FVector rgtPoint = FVector(point.X, point.Y+100, point.Z);
            drawLine(lftPoint, rgtPoint);
        }

        void drawLine(FVector point1, FVector point2)
        {
            DrawDebugLine(_pawn->GetWorld(), point1, point2, FColor::Green, false, .01, 0, 0.5);
        }

        void PostInitializeComponents()
        {
            // Add "Vehicle" tag for use by level blueprint
            _pawn->Tags.Add(FName("Vehicle"));

            if (_soundCue->IsValidLowLevelFast()) {
                _audioComponent->SetSound(_soundCue);
            }


            _vehicleBottom = _frameMesh->GetBoundingBox().Min.Z;
        }


        void rotateGimbal(FQuat rotation)
        {
            _springArm->SetRelativeRotation(rotation);
        }

}; // class Vehicle
