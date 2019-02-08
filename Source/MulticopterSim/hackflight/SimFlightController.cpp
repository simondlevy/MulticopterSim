/*
 * SimFlightController.cpp: Hackflight flight-control class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "SimFlightController.h"

#include "VehiclePawn.h"

#include "Joystick.h"

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include <hackflight.hpp>
#include "SimReceiver.h"
//
// MSP comms
#include "msppg/MSPPG.h"

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/althold.hpp>
#include <pidcontrollers/poshold.hpp>

// Mixer
#include <mixers/quadx.hpp>

// Additional sensors
#include "hackflight/sensors/SimOpticalFlow.h"
#include "hackflight/sensors/SimRangefinder.h"

class HackflightSimFlightController : public SimFlightController, public hf::Board {

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

        float _quat[4];
        float _gyro[3];

        float _motorvals[3];

        // Support for additional sensors
        //SimOpticalFlow _flowSensor = SimOpticalFlow(this);
        //SimRangefinder _rangefinder = SimRangefinder(this);

        // SimFlightController method implementation -----------------------------------

    public:

        HackflightSimFlightController(void)
        {
            // Start the "receiver" (joystick/gamepad)
            receiver = new hf::SimReceiver();

            // Start Hackflight firmware, indicating already armed
            hackflight.init(this, receiver, &mixer, &ratePid, true);

            // Add optical-flow sensor
            //hackflight.addSensor(&_flowSensor);

            // Add rangefinder
            //hackflight.addSensor(&_rangefinder);

            // Add level PID controller for aux switch position 1
            hackflight.addPidController(&level, 1);

            // Add loiter PID controllers for aux switch position 2
            hackflight.addPidController(&althold, 2);
            //hackflight.addPidController(&poshold, 2);
        }

        virtual void start(void) override
        {
            // Initialize time to a positive value to avod divide-by-zero
            _elapsedTime = 1.0;
        }

        virtual TArray<float> update(float timestamp, FVector position, FVector velocity, FQuat quat, FVector gyro, FVector accel) override
        {
            // Unused for Hackflight
            (void)position;
            (void)timestamp;
            (void)velocity;
            (void)accel;

            receiver->update();

            hackflight.update();

            // Store quaternion and gyro values for Hackflight::Board methods below
            _quat[0] = quat.W;
            _quat[1] = quat.X;
            _quat[2] = quat.Y;
            _quat[3] = quat.Z;
            _gyro[0] = gyro.X;
            _gyro[1] = gyro.Y;
			_gyro[2] = 0; // zero-out gyro Z for now

            TArray<float> motorvals = {_motorvals[0], _motorvals[1], _motorvals[2], _motorvals[3]};
            return motorvals;

        }

		virtual TArray<float> update(float timestamp, FVector position, FVector velocity) override
		{
			TArray<float> motorvals = { _motorvals[0], _motorvals[1], _motorvals[2], _motorvals[3] };
			return motorvals;
		}

    protected:

        // Hackflight::Board method implementation -------------------------------------

        virtual bool getQuaternion(float quat[4]) override
        {
            memcpy(quat, _quat, 4*sizeof(float));
            return true;
        }

        virtual bool getGyrometer(float gyro[3]) override
        {
            memcpy(gyro, _gyro, 3*sizeof(float));
            return true;
        }

        virtual void writeMotor(uint8_t index, float value) override
        {
            _motorvals[index] = value;
        }

        virtual float getTime(void) override
        {
            // Track elapsed time
            _elapsedTime += .01; // Assume 100Hz clock

            return _elapsedTime;
        }

        virtual uint8_t	serialAvailableBytes(void) override
        {
            return 0; // XXX
        }

        virtual uint8_t	serialReadByte(void) override
        {
            return 0; // XXX
        }

        virtual void serialWriteByte(uint8_t c) override
        { // XXX
        }

}; // HackflightSimFlightController

// Debugging
void hf::Board::outbuf(char * buf)
{
    AVehiclePawn::outbuf(buf);
}

// Factory method
SimFlightController * SimFlightController::createSimFlightController(void)
{
    return new HackflightSimFlightController();
}
