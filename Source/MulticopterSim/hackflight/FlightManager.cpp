/*
* FlightManager.cpp: MulticopterSim FlightManager class implementation using Hackflight
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "FlightManager.h"

#include "SimBoard.h"
#include "BuiltinPhysics.h"
#include "VehiclePawn.h"

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include <hackflight.hpp>
#include "hackflight/SimReceiver.h"

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/althold.hpp>
#include <pidcontrollers/poshold.hpp>

// Mixer
#include <mixers/quadx.hpp>

class HackflightFlightManager : public FlightManager {

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

        hf::AltitudeHold althold = hf::AltitudeHold(
                1.00f,  // altHoldP
                0.50f,  // altHoldVelP
                0.01f,  // altHoldVelI
                0.10f); // altHoldVelD

        hf::PositionHold poshold = hf::PositionHold(
                0.2,	// posP
                0.2f,	// posrP
                0.0f);	// posrI

        // Main firmware
        hf::Hackflight _hackflight;

        // Flight-controller board
        SimBoard _board;

        // "Receiver" (joystick/gamepad)
        SimReceiver _receiver;

        // Mixer
        hf::MixerQuadX _mixer;

    public:

        HackflightFlightManager(void)
        {
			// Start Hackflight firmware, indicating already armed
			_hackflight.init(&_board, &_receiver, &_mixer, &ratePid, true);

			// Add level PID controller for aux switch position 1
			_hackflight.addPidController(&level, 1);
		}

        ~HackflightFlightManager(void)
        {
        }

        virtual TArray<float> update(float currentTime, FQuat quat, FVector gyro) override
        {
			_receiver.update();

			_hackflight.update();

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
