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

            _receiver->poll();
        }

        void tick(void)
        {
        }

}; // FHackflightFlightManager
