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
#include "Vehicle.hpp"

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

        Vehicle * _vehicle = NULL;

        // Gimbal camera support
        class USpringArmComponent* _gimbalSpringArm;
        class UCameraComponent* _camera;
        class USceneCaptureComponent2D * _capture;


        // Threaded worker for managing video from camera
        class FVideoManager * _videoManager = NULL;
        void videoManagerStart(void);
        void videoManagerStop(void);
        void videoManagerGrabImage(void);

        // Bozo filter for failure to select a map
        bool _mapSelected = false;

        // Render targets, passed to consgtructor for threaded video worker when Start button is pressed
        UTextureRenderTarget2D * _cameraRenderTarget;

        // Sets axes for camera gimbal based on values returned in child class
        void setGimbal(void);

        // Switch cameras perioically for testing
        void switchCameras(float DeltaSeconds);

        // Set camera field of view
        void setCameraFOV(float cameraFieldOfView, float captureFOVAngle);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override;

    public:	

        ABigQuadPawn();

        ~ABigQuadPawn();

}; // ABigQuadPawn
