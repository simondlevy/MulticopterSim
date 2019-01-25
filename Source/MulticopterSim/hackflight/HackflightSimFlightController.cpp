/*
 * HackflightSimFlightController.cpp: Hackflight flight-control class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "VehiclePawn.h"

#include "SimFlightController.h"

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

// Additional sensors
#include "hackflight/sensors/SimOpticalFlow.h"
#include "hackflight/sensors/SimRangefinder.h"

// PID tuning

static hf::Rate ratePid = hf::Rate(
        0.01,	// Roll/Pitch P
        0.01,	// Roll/Pitch I
        0.01,	// Roll/Pitch D
        0.5,	// Yaw P
        0.0,	// Yaw I
        8.f);	// Demands to rate


hf::Level level = hf::Level(0.20f);

#ifdef _PYTHON
static PythonLoiter loiter = PythonLoiter(
        0.5f,	// Altitude P
        1.0f,	// Altitude D
        0.2f);	// Cyclic P
#else

static hf::AltitudeHold althold = hf::AltitudeHold(
        1.00f,  // altHoldP
        0.50f,  // altHoldVelP
        0.01f,  // altHoldVelI
        0.10f); // altHoldVelD

static hf::PositionHold poshold = hf::PositionHold(
        0.2,	// posP
        0.2f,	// posrP
        0.0f);	// posrI

#endif

#include <mixers/quadx.hpp>

class HackflightSimFlightController : public SimFlightController, public hf::Board {

    // SimFlightController method implementation -----------------------------------

    public:

        virtual void init(void) override
        {
            joystick.init();

            receiver = new hf::SimReceiver(joystick.axismap, joystick.buttonmap, joystick.reversedVerticals, joystick.springyThrottle, joystick.useButtonForAux);

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

            // Initialize time to a positive value to avod divide-by-zero
            _elapsedTime = 1.0;
        }

        virtual void update(float quat[4], float gyro[3], float motorvals[4]) override
        {
            joystick.poll();

            receiver->update(joystick.axes, joystick.buttons);

            hackflight.update();

            memcpy(_quat, quat, 4*sizeof(float));
            memcpy(_gyro, gyro, 3*sizeof(float));

            memcpy(motorvals, _motorvals, 4*sizeof(float));
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

    private:

        // Main firmware
        hf::Hackflight hackflight;

        // Receiver (joystick)
        hf::SimReceiver * receiver;

        // Mixer
        hf::MixerQuadX mixer;

        // Joystick
        Joystick joystick;

        float _elapsedTime;

        float _quat[4];
        float _gyro[3];

        float _motorvals[3];

        // Support for additional sensors
        //SimOpticalFlow _flowSensor = SimOpticalFlow(this);
        //SimRangefinder _rangefinder = SimRangefinder(this);

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
