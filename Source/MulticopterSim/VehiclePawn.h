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

// Support for adding Gaussian noise to sensors
#include <random>

#include "ThreadedSocketServer.h"

#include "SimFlightController.h"

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
        SimFlightController * flightController;

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

        // Debugging
        void debug(char * fmt, ...);

        // Joystick support -------------------------------------------------------

        static const uint16_t VENDOR_STM	        = 0x0483;

        static const uint16_t PRODUCT_TARANIS		= 0x5710;
        static const uint16_t PRODUCT_PS3_CLONE		= 0x0003;
        static const uint16_t PRODUCT_XBOX360_CLONE	= 0xfafe;
        static const uint16_t PRODUCT_EXTREMEPRO3D	= 0xc215;
        static const uint16_t PRODUCT_F310	        = 0xc21d;
        static const uint16_t PRODUCT_PS4	        = 0x09cc;

        int  _joyid;

        void joystickInit(SimFlightController * flightController);
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

        AVehiclePawn();

        // AActor overrides
        virtual void BeginPlay() override;
        virtual void PostInitializeComponents() override;
        virtual void Tick(float DeltaSeconds) override;
        virtual void NotifyHit(class UPrimitiveComponent* MyComp, class AActor* Other, class UPrimitiveComponent* OtherComp, 
                bool bSelfMoved, FVector HitLocation, FVector HitNormal, FVector NormalImpulse, const FHitResult& Hit) override;
        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // Debugging support
        static void outbuf(char * buf);

        // Returns PlaneMesh subobject 
        FORCEINLINE class UStaticMeshComponent* GetPlaneMesh() const { return PlaneMesh; }
};
