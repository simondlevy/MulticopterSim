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

#include "Physics.h"
#include "GaussianNoise.h"

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

        // Actual  physics supported by your application
        class Physics * _physics;

        // Support for simulating spinning propellers
        uint8_t _tickCycle;
        static constexpr int8_t MOTORDIRS[4] = {+1, -1, -1, +1};
		static constexpr uint8_t PROP_UPDATE = 5;

		// Bozo filter for failure to select a map
		bool _mapSelected;

        // Animation effects (sound, spinning props)
        void addAnimationEffects(TArray<float> motorvals);
        void setAudioPitchAndVolume(float value);

protected:

	    // AActor overrides
        virtual void BeginPlay() override;
        virtual void Tick(float DeltaSeconds) override;
        virtual void PostInitializeComponents() override;
        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;
        virtual void NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
                bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit) override;

public:	

        // Sets default values for this pawn's properties
        AVehiclePawn();

        // Debugging support
        static void debug(const char * fmt, ...);
        static void outbuf(char * buf);

}; // AVehiclePawn
