/*
 * Header-only support for vehicle pawns in MulticopterSim
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

// A macro for simplifying the declaration of static meshes
#define DECLARE_STATIC_MESH(structname, assetstr, objname)   \
    static struct structname {                                             \
        ConstructorHelpers::FObjectFinderOptional<UStaticMesh> mesh;   \
        structname() : mesh(TEXT("/Game/Flying/Meshes/" assetstr)) { } \
    };                                                                     \
    static structname objname;

class MULTICOPTERSIM_API Vehicle {


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
                    //_propellerMeshComponents[i]->SetRelativeRotation(FRotator(0, rotation * _propellerDirections[i] * 100, 0));
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
        APawn * _pawn;

        // Dynamics
        MultirotorDynamics * _dynamics;

        // Flight management thread
        void startThreadedWorkers(void)
        {
            _flightManager = FFlightManager::create(_dynamics, _startLocation, _startRotation);
        }        

        void stopThreadedWorkers(void)
        {
            _flightManager = (FFlightManager *)FThreadedWorker::stopThreadedWorker(_flightManager);
        }

    public:

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
        Vehicle(APawn * pawn, UStaticMesh * frameMesh, MultirotorDynamics * dynamics, uint8_t motorCount)
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
            _dynamics = dynamics;
        }        

        ~Vehicle(void) 
        {
            //delete _propellerMeshComponents;
            //delete _propellerDirections;
            //delete _dynamics;
            //delete _motorvals;
        }

        void addMotor( uint8_t index, int8_t direction, const wchar_t * mMeshName, UStaticMesh * mMesh, FVector mLocation,
                const wchar_t * pMeshName, UStaticMesh * pMesh, FVector pLocation)
        {
            UStaticMeshComponent * mMeshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(mMeshName);
            mMeshComponent->SetStaticMesh(mMesh);
            mMeshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName); 	
            mMeshComponent->AddRelativeLocation(mLocation);

            UStaticMeshComponent * pMeshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(pMeshName);
            pMeshComponent->SetStaticMesh(pMesh);
            pMeshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName);
            pMeshComponent->AddRelativeLocation(pLocation);

            _propellerMeshComponents[index] = pMeshComponent;
            _propellerDirections[index] = direction;
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

                // OSD for debugging messages from threaded workers
                debug("%s", _flightManager->getMessage());
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

        void getGimbal(float & roll, float & pitch)
        {
            _flightManager->getGimbal(roll, pitch);
        }

}; // Vehicle
