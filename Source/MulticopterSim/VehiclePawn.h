/*
* VehiclePawn.h: Class declaration for pawn class in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#define WIN32_LEAN_AND_MEAN

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include "SimFlightController.h"
#include "GaussianNoise.h"
#include "VehiclePhysics.h"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "VehiclePawn.generated.h"

UCLASS(Config=Game)
class AVehiclePawn : public APawn
{
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

        // FPV camera support
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = Camera, meta = (AllowPrivateAccess = "true"))
            class UCameraComponent* _fpvCamera;
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = Camera, meta = (AllowPrivateAccess = "true"))
            class USpringArmComponent* _fpvSpringArm;

        // Abstract controller
        class SimFlightController * _flightController;

        // Support for simulating spinning propellers
        uint8_t _tickCycle;
        static constexpr int8_t MOTORDIRS[4] = {+1, -1, -1, +1};
		static constexpr uint8_t PROP_UPDATE = 5;

        // Support for sensor emulation via first differencing
        FVector _eulerPrev;
        float _varioPrev;
        float _groundAltitude;
        float _elapsedTime;

		// Bozo filter for failure to select a map
		bool _mapSelected;

        // Animation effects (sound, spinning props)
        void addAnimationEffects(TArray<float> motorvals);
        void setAudioPitchAndVolume(float value);
        void silenceAudio(void);

        // Simulate Gaussian sensor noise
        GaussianNoise _gyroNoise  = GaussianNoise(3, .001);  // radians / second
        GaussianNoise _accelNoise = GaussianNoise(3, .001);  // Gs / second
        GaussianNoise _baroNoise  = GaussianNoise(1, 5.0);   // pascals / second
        GaussianNoise _quatNoise  = GaussianNoise(4, 0);     // [+/-1]
        GaussianNoise _rangeNoise = GaussianNoise(1, .002);  // meters
        GaussianNoise _flowNoise  = GaussianNoise(2, .001);  // meters / second

        // Support various models of vehicle physics
        VehiclePhysics * _vehiclePhysics;

        // Simulate IMU via ground-truth
        FVector getGyrometer(FVector & euler, float DeltaSeconds);

		static float mean(TArray<float> x);

    public:

        AVehiclePawn();

        ~AVehiclePawn();

		// Earth's gravity
		static constexpr float G = 9.80665f;

        // AActor overrides
        virtual void BeginPlay() override;
        virtual void PostInitializeComponents() override;
        virtual void Tick(float DeltaSeconds) override;
        virtual void NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
                bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit) override;
        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // Debugging support
        static void debug(char * fmt, ...);
        static void outbuf(char * buf);

        // Returns _vehicleMesh subobject 
        FORCEINLINE class UStaticMeshComponent* GetPlaneMesh() const { return _vehicleMesh; }
};
