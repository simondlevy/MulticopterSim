/*
* Class declaration for pawn class in MulticopterSim
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

#include "FlightManager.hpp"

#ifdef _USE_OPENCV
#include "VideoManager.hpp"
#endif

#include "dynamics/QuadXAPDynamics.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "BigQuad.generated.h"

UCLASS(Config=Game)
class MULTICOPTERSIM_API ABigQuadPawn : public APawn {

    private:

        GENERATED_BODY()

        // Physical constants ==============================================

        // Reverse-engineered so that sqrt(x^2+y^2) = 0.6
        static constexpr double _motorLocations[12] = 
        {
            // X     Y    Z
            -.42, +.42, +.1, 
            +.42, +.42, +.1, 
            -.42, -.42, +.1, 
            +.42, -.42, +.1 
        };

        static constexpr MultirotorDynamics::params_t _params = {

            // Amir's calculations
            5.30216718361085E-05,   // b
            2.23656692806239E-06,   // d
            16.47,                  // m
            0.6,                    // l
            2,                      // Ix
            2,                      // Iy
            3,                      // Iz
            3.08013E-04,            // Jr

            // maxrpm, estimated
            15000                  
        }; 

        // =================================================================

        // StaticMesh component that will be the visuals for our flying pawn 
        class UStaticMeshComponent* _vehicleMesh;

        // Audio support: see http://bendemott.blogspot.com/2016/10/unreal-4-playing-sound-from-c-with.html
        class USoundCue* _propellerAudioCue;
        class USoundCue* _propellerStartupCue;
        class UAudioComponent* _propellerAudioComponent;

        // Gimbal camera support
        class USpringArmComponent* _gimbalSpringArm;
        class UCameraComponent* _camera;
        class USceneCaptureComponent2D * _capture;


        // Threaded workers for running flight control, video
        class FFlightManager * _flightManager = NULL;

        // Threaded workers for running flight control, video
        class FVideoManager * _videoManager = NULL;

        // Bozo filter for failure to select a map
        bool _mapSelected = false;

        // Motor values for animation/sound
        float * _motorvals = NULL;

        // Retrieves kinematics from dynamics computed in another thread
        void getKinematics(void);

        // Animation effects (sound, spinning props)
        void addAnimationEffects(void);
        void setAudioPitchAndVolume(float value);

        // Implement for each vehicle mesh
        static const uint8_t getMotorCount(void);
        static const int8_t  getMotorDirection(uint8_t j);
        static const char ** getPropellerMeshNames(void);

        // Render targets, passed to consgtructor for threaded video worker when Start button is pressed
        UTextureRenderTarget2D * _cameraRenderTarget;

        // Sets axes for camera gimbal based on values returned in child class
        void setGimbal(void);

        // Switch cameras perioically for testing
        void switchCameras(float DeltaSeconds);

        // Starting pose for reset on crash
        FVector _startLocation;
        FRotator _startRotation;

        // Frame configuration
        QuadXAPDynamics * _dynamics;

        // Flight management thread
        void startThreadedWorkers(void);
        void stopThreadedWorkers(void);

        // Helper
        static bool childComponentHasName(UStaticMeshComponent * child, const char * fmt, int index);

        // Set camera field of view
        void setCameraFOV(float cameraFieldOfView, float captureFOVAngle);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        virtual void NotifyHit(
                class UPrimitiveComponent* MyComp, 
                class AActor* Other, 
                class UPrimitiveComponent* OtherComp, 
                bool bSelfMoved, 
                FVector HitLocation, 
                FVector HitNormal, 
                FVector NormalImpulse, 
                const FHitResult& Hit) override;

    public:	

        ABigQuadPawn();

        ~ABigQuadPawn();

}; // ABigQuadPawn
