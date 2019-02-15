// Fill out your copyright notice in the Description page of Project Settings.


/*
* FlightManager.cpp: MulticopterSim FlightManager class implementation using Hackflight
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "FlightManager.h"

//#include "SimBoard.h"
#include "BuiltinPhysics.h"
#include "VehiclePawn.h"
//#include "Joystick.h"

/*
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
*/

class HackflightFlightManager : public FlightManager {

    private:

        /*
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
        hf::Hackflight _hackflight;

        // Flight-controller board
        SimBoard _board;

        // "Receiver" (joystick/gamepad)
        hf::SimReceiver * _receiver;

        // Mixer
        hf::MixerQuadX _mixer;
*/
    public:

        HackflightFlightManager(void)
        {
			// Start the "receiver" (joystick/gamepad)
			//_receiver = new hf::SimReceiver();

			// Start Hackflight firmware, indicating already armed
			//_hackflight.init(&_board, _receiver, &_mixer, &ratePid, true);

			// Add level PID controller for aux switch position 1
			//_hackflight.addPidController(&level, 1);

			// Add loiter PID controllers for aux switch position 2
			//_hackflight.addPidController(&althold, 2);
			//hackflight.addPidController(&poshold, 2);
		}

        ~HackflightFlightManager(void)
        {
        }

        virtual TArray<float> update(float currentTime, FQuat quat, FVector gyro) override
        {
			//_receiver->update();

			//_hackflight.update();

            //return _board.update(currentTime, quat, gyro);

            TArray<float> motorvals = {0,0,0,0};
            return motorvals;

        }

}; // HackflightFlightManager


// Factory method for FlightManager class
FlightManager * FlightManager::createFlightManager()
{
    return new HackflightFlightManager();
}

/*
// Debugging
void hf::Board::outbuf(char * buf)
{
    AVehiclePawn::outbuf(buf);
}
*/
