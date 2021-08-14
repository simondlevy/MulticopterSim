/*
   MulticopterSim FlightManager class implementation using UDP sockets 

   Acts as a client for a server program running on another address

   Sends joystick demands and vehicle state to server; receives motor
   values

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include <HF_pure.hpp>

// PID controllers
#include <hf_pidcontrollers/level.hpp>
#include <hf_pidcontrollers/rate.hpp>
#include <hf_pidcontrollers/yaw.hpp>
#include <hf_pidcontrollers/althold.hpp>
#include <hf_pidcontrollers/poshold.hpp>

#include "SimReceiver.hpp"
#include "SimBoard.hpp"
#include "SimSensors.hpp"
#include "SimMotor.hpp"

class FHackflightFlightManager : public FFlightManager {

    private:

        // PID controllers
		hf::RatePid _ratePid = hf::RatePid(0.225, 0.001875, 0.375);
		hf::YawPid _yawPid = hf::YawPid(1.0625, 0.005625);
        hf::LevelPid _levelPid = hf::LevelPid(0.20);
        hf::AltitudeHoldPid _altHoldPid;
        hf::PositionHoldPid _posHoldPid;

        // Mixer
        hf::Mixer * _mixer = NULL;

        // "Board": implements getTime()
        SimBoard _board;

        // "Receiver": joystick/gamepad
        SimReceiver * _receiver = NULL;

        // "Sensors": directly from ground-truth
        SimSensors * _sensors = NULL;

        // "Motors": passed to mixer so it can modify them
        SimMotor * _motors[100] = {};

        // Main firmware
        hf::Hackflight * _hackflight = NULL;
        
        // Guards socket comms
        bool _ready = false;

    public:

        FHackflightFlightManager(
                APawn * pawn,
                hf::Mixer * mixer,
                SimMotor ** motors,
                Dynamics * dynamics)
            : FFlightManager(dynamics)
        {
            // Store mixer, motors for later
            _mixer = mixer;
            for (uint8_t k=0; k<_actuatorCount; ++k) {
                _motors[k] = motors[k];
            }

            // Pass PlayerController to receiver constructor in case we have no
            // joystick / game-controller
            _receiver = new SimReceiver(pawn);

            // Create Hackflight object
            _hackflight = new hf::Hackflight(&_board, _receiver, _mixer);

            // Add simulated sensor suite
            _sensors = new SimSensors(_dynamics);
            _hackflight->addSensor(_sensors);
 
            // Add PID controllers for all aux switch positions.
            // Position hold goes first, so it can have access to roll and yaw
            // stick demands before other PID controllers modify them.
            _hackflight->addClosedLoopController(&_posHoldPid);
            _hackflight->addClosedLoopController(&_ratePid);
            _hackflight->addClosedLoopController(&_yawPid);
            _hackflight->addClosedLoopController(&_levelPid);
            _hackflight->addClosedLoopController(&_altHoldPid);

            // Start Hackflight firmware, indicating already armed
            _hackflight->begin(true);

            _ready = true;
        }

        ~FHackflightFlightManager()
        {
        }

        virtual void getActuators(const double time, double * values) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!_ready) {
                return;
            }

            // Poll the "receiver" (joystick or game controller)
            _receiver->poll();

            // Update the Hackflight firmware, causing Hackflight's actuator
            // to set the values of the simulated motors
            _hackflight->update();
        }

        void tick(void)
        {
        }

}; // FHackflightFlightManager
