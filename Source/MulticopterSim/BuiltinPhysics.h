/*
 * BuiltinPhysics.h: Physics class using UE4 built-in physics 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Physics.h"
#include "VehiclePawn.h"
#include "Joystick.h"

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include <hackflight.hpp>
#include "hackflight/SimReceiver.h"

// MSP comms
#include "hackflight/msppg/MSPPG.h"

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/althold.hpp>
#include <pidcontrollers/poshold.hpp>

// Mixer
#include <mixers/quadx.hpp>

// Additional sensors
#include "hackflight/sensors/SimOpticalFlow.h"
#include "hackflight/sensors/SimRangefinder.h"

class BuiltinPhysics : public Physics, public hf::Board {

    private:

        // PID tuning

        hf::Rate ratePid = hf::Rate(
                0.01,	// Roll/Pitch P
                0.01,	// Roll/Pitch I
                0.01,	// Roll/Pitch D
                0.5,	// Yaw P
                0.0,	// Yaw I
                8.f);	// Demands to rate


        hf::Level level = hf::Level(0.20f);

#ifdef _PYTHON
        PythonLoiter loiter = PythonLoiter(
                0.5f,	// Altitude P
                1.0f,	// Altitude D
                0.2f);	// Cyclic P
#else

        hf::AltitudeHold althold = hf::AltitudeHold(
                1.00f,  // altHoldP
                0.50f,  // altHoldVelP
                0.01f,  // altHoldVelI
                0.10f); // altHoldVelD

        hf::PositionHold poshold = hf::PositionHold(
                0.2,	// posP
                0.2f,	// posrP
                0.0f);	// posrI

#endif

        // Main firmware
        hf::Hackflight hackflight;

        // "Receiver" (joystick/gamepad)
        hf::SimReceiver * receiver;

        // Mixer
        hf::MixerQuadX mixer;

        float _elapsedTime;

        float _eulerXPrev;
        float _eulerYPrev;

        float _quat[4];
        float _gyro[3];

        float _motorvals[3];

        // Support for sensor emulation via first differencing
        float _varioPrev;
        float _groundAltitude;

        FVector getAccelerometer(float velocityZ, FVector & euler, float deltaSeconds);

        FQuat getQuaternion(class AVehiclePawn * vehiclePawn);

    public:

        BuiltinPhysics(void);

        virtual void start(void) override;

        virtual TArray<float> update(float deltaSeconds, AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh) override;

        void computeForces(float deltaSeconds, TArray<float> motorValues, FVector & euler, FVector & rotationForce, FVector & translationForce);

        static constexpr float THRUST_FACTOR = 130.f;

        static float motorsToAngularForce(TArray<float> motorValues, uint8_t a, uint8_t b, uint8_t c, uint8_t d);

        static float sum(TArray<float> x);

    protected:

            // Hackflight::Board method implementation -------------------------------------

            virtual bool getQuaternion(float quat[4]) override;

            virtual bool getGyrometer(float gyro[3]) override;

            virtual void writeMotor(uint8_t index, float value) override;

            virtual float getTime(void) override;

            virtual uint8_t	serialAvailableBytes(void) override;

            virtual uint8_t	serialReadByte(void) override;

            virtual void serialWriteByte(uint8_t c) override;

}; // BuiltinPhysics

