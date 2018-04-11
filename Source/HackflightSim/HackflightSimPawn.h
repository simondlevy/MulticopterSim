// Copyright 1998-2017 Epic Games, Inc. All Rights Reserved.
#pragma once

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include <hackflight.hpp>
using namespace hf;

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"
#include "Runtime/Engine/Classes/Sound/SoundCue.h"
#include "Components/AudioComponent.h"
#include "HackflightSimPawn.generated.h"

UCLASS(Config=Game)
class AHackflightSimPawn : public APawn, public Board
{
    private:

        GENERATED_BODY()

        // StaticMesh component that will be the visuals for our flying pawn 
        UPROPERTY(Category = Mesh, VisibleDefaultsOnly, BlueprintReadOnly, meta = (AllowPrivateAccess = "true"))
        class UStaticMeshComponent* PlaneMesh;

        // Propeller meshes for spinning
        class UStaticMeshComponent* PropMeshes[4];

        // Audio support: see http://bendemott.blogspot.com/2016/10/unreal-4-playing-sound-from-c-with.html

        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Audio", meta = (AllowPrivateAccess = "true"))
            class USoundCue* propellerAudioCue;
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Audio", meta = (AllowPrivateAccess = "true"))
            class USoundCue* propellerStartupCue;
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = "Audio", meta = (AllowPrivateAccess = "true"))
            class UAudioComponent* propellerAudioComponent;

        // FPV camera support
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = Camera, meta = (AllowPrivateAccess = "true"))
            class UCameraComponent* fpvCamera;
        UPROPERTY(VisibleAnywhere, BlueprintReadOnly, Category = Camera, meta = (AllowPrivateAccess = "true"))
            class USpringArmComponent* fpvSpringArm;

    public:

        AHackflightSimPawn();

        // Begin AActor overrides
        virtual void BeginPlay() override;
        void PostInitializeComponents() override;
        virtual void Tick(float DeltaSeconds) override;
        virtual void NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
                bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit) override;
        // End AActor overrides

        virtual void     init(void) override;
        virtual bool     getEulerAngles(float eulerAngles[3]) override;
        virtual bool     getGyroRates(float gyroRates[3]) override;
        virtual uint32_t getMicroseconds() override;
        virtual void     writeMotor(uint8_t index, float value) override;

        // Support for spinning propellers
        const int8_t motordirs[4] = {+1, -1, -1, +1};
        float motorvals[4];

        // Support for Hackflight::Board::getMicroseconds()
        float elapsedTime;

        // Converts a set of motor values to angular forces in body frame
        float motorsToAngularForce(int a, int b, int c, int d);

        // Returns PlaneMesh subobject 
        FORCEINLINE class UStaticMeshComponent* GetPlaneMesh() const { return PlaneMesh; }
};
