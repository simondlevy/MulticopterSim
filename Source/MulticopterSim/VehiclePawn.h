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

#include "FlightManager.h"
#include "dynamics/MultirotorDynamics.h"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "VehiclePawn.generated.h"

UCLASS(Config=Game)
class MULTICOPTERSIM_API AVehiclePawn : public APawn {

    friend class FlightManager;

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

        // Threaded worker for running flight control
        class FFlightManager * _flightManager;

		// Bozo filter for failure to select a map
		bool _mapSelected;

        // Interact with flight manager
        void getPoseAndMotors(float deltaSeconds);

        // Animation effects (sound, spinning props)
        void addAnimationEffects(void);
        void setAudioPitchAndVolume(float value);

        // Implement for each vehicle mesh
        static const uint8_t getMotorCount(void);
        static const int8_t  getMotorDirection(uint8_t j);
        static const char ** getPropellerMeshNames(void);

        // Motor values for animation/sound
        double * _motorvals;

        // XXX we should be able to get rid of this once we've debugged the dynamics sufficiently
        bool sanityCheck(double v[3]);

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

        // Sets axes for camera gimbal
        void setGimbal(float roll, float pitch, float yaw);

        // Timing
        float getCurrentTime(void);

}; // AVehiclePawn
