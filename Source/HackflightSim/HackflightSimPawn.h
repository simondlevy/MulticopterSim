/*
* HackflightSimPawn.h: Class declaration for pawn class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

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
// Override both APawn and Hackflight::Board to simplify the API
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

        // Support for spinning propellers
        const int8_t motordirs[4] = {+1, -1, -1, +1};
        float motorvals[4];

        // Support for quaternions
        FQuat quat;

        // Support for accelerometer, gyrometer emulation
        FVector eulerPrev;
        FVector gyro;

        // Converts a set of motor values to angular forces in body frame
        float motorsToAngularForce(int a, int b, int c, int d);

        // Supports MSP over socket
        void serverError(void);
        bool serverRunning;

    public:

        AHackflightSimPawn();

        // AActor overrides
        virtual void BeginPlay() override;
        virtual void PostInitializeComponents() override;
        virtual void Tick(float DeltaSeconds) override;
        virtual void NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
                bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit) override;
		virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // Hackflight::Board overrides
        virtual bool     getQuaternion(float quat[4]) override;
        virtual bool     getGyrometer(float gyroRates[3]) override;
        virtual void     writeMotor(uint8_t index, float value) override;

        // Returns PlaneMesh subobject 
        FORCEINLINE class UStaticMeshComponent* GetPlaneMesh() const { return PlaneMesh; }
};
