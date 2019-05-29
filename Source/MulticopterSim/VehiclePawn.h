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

#ifdef __USE_OPENCV
#include "VideoManagerOpenCV.hpp"
#else
#include "VideoManagerStub.hpp"
#endif

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "VehiclePawn.generated.h"

UCLASS(Config=Game)
class MULTICOPTERSIM_API AVehiclePawn : public APawn {

private:

	GENERATED_BODY()

        // StaticMesh component that will be the visuals for our flying pawn 
        UPROPERTY(Category = Mesh, VisibleDefaultsOnly, BlueprintReadOnly, meta = (AllowPrivateAccess = "true"))
            class UStaticMeshComponent* _vehicleMesh;

        // Propeller meshes for spinning
        class UStaticMeshComponent* _propMeshes[4];

        // Audio support: see http://bendemott.blogspot.com/2016/10/unreal-4-playing-sound-from-c-with.html
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Audio", meta = (AllowPrivateAccess = "true"))
            class USoundCue* _propellerAudioCue;
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Audio", meta = (AllowPrivateAccess = "true"))
            class USoundCue* _propellerStartupCue;
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Audio", meta = (AllowPrivateAccess = "true"))
            class UAudioComponent* _propellerAudioComponent;

        // Camera support
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = Camera, meta = (AllowPrivateAccess = "true"))
            class UCameraComponent* _fpvCamera;
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = Camera, meta = (AllowPrivateAccess = "true"))
            class USpringArmComponent* _fpvSpringArm;

        // Threaded workers for running flight control, video
        class FFlightManager * _flightManager = NULL;
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
        UTextureRenderTarget2D * _camera1RenderTarget;
        UTextureRenderTarget2D * _camera2RenderTarget;

        // Sets axes for camera gimbal based on values returned in child class
        void setGimbal(void);

        // Starting pose for reset on crash
        FVector _startLocation;
        FRotator _startRotation;

        // Flight management thread
        void startThreadedWorkers(void);
        void stopThreadedWorkers(void);

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

        AVehiclePawn();

        ~AVehiclePawn();

}; // AVehiclePawn
