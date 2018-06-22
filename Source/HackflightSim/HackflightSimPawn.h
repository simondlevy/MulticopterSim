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

#include "ThreadedSocketServer.h"

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
        float _motorvals[4];

        // Support for quaternions
        FQuat _quat;

        // Support for accelerometer, gyrometer emulation
        FVector _eulerPrev;
        FVector _gyro;

		// Support for altitude, vario
		float _altitude;
		float _altitudePrev;
		float _vario;

        // Converts a set of motor values to angular forces in body frame
        float motorsToAngularForce(int a, int b, int c, int d);

        // Supports MSP over socket
        void serverError(void);
        bool _serverRunning;
		int _serverAvailableBytes;
		int _serverByteIndex;
		char _serverBuffer[ThreadedSocketServer::BUFLEN];

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
        virtual bool    getQuaternion(float quat[4]) override;
        virtual bool    getGyrometer(float gyroRates[3]) override;
        virtual void    writeMotor(uint8_t index, float value) override;
        virtual uint8_t serialAvailableBytes(void) override;
        virtual uint8_t serialReadByte(void) override;
        virtual void    serialWriteByte(uint8_t c) override;

 		//virtual bool    getGroundTruth(vehicleState_t & state) override;

        // Returns PlaneMesh subobject 
        FORCEINLINE class UStaticMeshComponent* GetPlaneMesh() const { return PlaneMesh; }
};
