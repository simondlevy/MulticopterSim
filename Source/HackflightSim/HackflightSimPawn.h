/*
* HackflightSimPawn.h: Class declaration for pawn class in HackflightSim
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

// Support for adding Gaussian noise to sensors
#include <random>

#define _SIM
#include <hackflight.hpp>
using namespace hf;

// SimReceiver
#include "HackflightSimReceiver.h"

#include "ThreadedSocketServer.h"

#include "HackflightSimController.h"

#include <sensors/SimOpticalFlow.h>
#include <sensors/HackflightSimRangefinder.h>

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

        // Abstract controller
        HackflightSimController * controller;

        // Receiver (joystick)
        hf::SimReceiver * receiver;

        // Support for spinning propellers
        const int8_t motordirs[4] = {+1, -1, -1, +1};
        float _motorvals[4];

        // Support for quaternions
        FQuat _quat;

        // Support for sensor emulation
        FVector _eulerPrev;
        FVector _gyro;
		float _accelZ;
		float _varioPrev;
		float _groundAltitude;
		float _elapsedTime;

        // Converts a set of motor values to angular forces in body frame
        float motorsToAngularForce(int a, int b, int c, int d);

        // Supports MSP over socket
        void serverError(void);
        bool _serverRunning;
		int _serverAvailableBytes;
		int _serverByteIndex;
		char _serverBuffer[ThreadedSocketServer::BUFLEN];

		// Helpers
		FVector getEulerAngles(void);

		// Support for optical flow
		SimOpticalFlow _flowSensor = SimOpticalFlow(this);

		// Support for rangefinder
		HackflightSimRangefinder _rangefinder = HackflightSimRangefinder(this);

        // Joystick support -------------------------------------------------------

        static const uint16_t VENDOR_STM	        = 0x0483;

        static const uint16_t PRODUCT_TARANIS		= 0x5710;
        static const uint16_t PRODUCT_PS3_CLONE		= 0x0003;
        static const uint16_t PRODUCT_XBOX360_CLONE	= 0xfafe;
        static const uint16_t PRODUCT_EXTREMEPRO3D	= 0xc215;
        static const uint16_t PRODUCT_F310	        = 0xc21d;
        static const uint16_t PRODUCT_PS4	        = 0x09cc;

        int  _joyid;

        void joystickInit();
        void joystickPoll(void);


         // Helps us simulate sensor noise.   -------------------------------------
         // XXX We should simulate ODR (output data rates) as well, but 
        // UE4 frame rate is currently to slow to do that realistically.
        class GaussianNoise {

            public:

                GaussianNoise(uint8_t size, float noise);

                void addNoise(float vals[]);

            private:

                std::default_random_engine _generator;
                std::normal_distribution<float> _dist;


                uint8_t _size;
                float   _noise;
        };

        // Simulate Gaussian sensor noise
        GaussianNoise _gyroNoise  = GaussianNoise(3, .001);  // radians / second
        GaussianNoise _accelNoise = GaussianNoise(3, .001);  // Gs / second
        GaussianNoise _baroNoise  = GaussianNoise(1, 5.0);   // pascals / second
        GaussianNoise _quatNoise  = GaussianNoise(4, 0);     // [+/-1]
        GaussianNoise _rangeNoise = GaussianNoise(1, .002);  // meters
        GaussianNoise _flowNoise  = GaussianNoise(2, .001);  // meters / second

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
        virtual bool	getQuaternion(float quat[4]) override;
        virtual bool	getGyrometer(float gyroRates[3]) override;
        virtual void	writeMotor(uint8_t index, float value) override;
        virtual float   getTime(void) override;
        virtual uint8_t	serialAvailableBytes(void) override;
        virtual uint8_t	serialReadByte(void) override;
        virtual void	serialWriteByte(uint8_t c) override;

        // Returns PlaneMesh subobject 
        FORCEINLINE class UStaticMeshComponent* GetPlaneMesh() const { return PlaneMesh; }
};
