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
#include "SimMotor.hpp"

#include "sensors/SimGyrometer.hpp"
#include "sensors/SimQuaternion.hpp"
#include "sensors/SimAltimeter.hpp"
#include "sensors/SimOpticalFlow.hpp"

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
        SimGyrometer * _gyrometer = NULL;
        SimQuaternion * _quaternion = NULL;
        SimAltimeter * _altimeter = NULL;
        SimOpticalFlow * _opticflow = NULL;

        // "Motors": passed to mixer so it can modify them
        SimMotor * _motors[100] = {};

        // Main firmware
        hf::HackflightPure * _hackflight = NULL;
        
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
            _hackflight = new hf::HackflightPure(&_board, _receiver, _mixer);

            // Create simulated sensors
            _gyrometer = new SimGyrometer(_dynamics);
            _quaternion = new SimQuaternion(_dynamics);
            _altimeter = new SimAltimeter(_dynamics);
            _opticflow = new SimOpticalFlow(_dynamics);

            // Add simulated sensors
            _hackflight->addSensor(_gyrometer);
            _hackflight->addSensor(_quaternion);
            _hackflight->addSensor(_altimeter);
            _hackflight->addSensor(_opticflow);
 
            // Add PID controllers for all aux switch positions.
            // Position hold goes first, so it can have access to roll and yaw
            // stick demands before other PID controllers modify them.
            _hackflight->addPidController(&_posHoldPid);
            _hackflight->addPidController(&_ratePid);
            _hackflight->addPidController(&_yawPid);
            _hackflight->addPidController(&_levelPid);
            _hackflight->addPidController(&_altHoldPid);

            // Start Hackflight firmware, indicating already armed
            _hackflight->begin();

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

            // Set the time in the simulated board, so it can be retrieved by
            // Hackflight
            _board.set(time);

            //  Get the new motor values
            for (uint8_t i=0; i < _actuatorCount; ++i) {
                values[i] = _motors[i]->getValue();
            }
            //debugline("m1: %3.3f  m2: %3.3f  m3: %3.3f  m4: %3.3f",   
            //        values[0], values[1], values[2], values[3]);
        }

        void tick(void)
        {
        }

}; // FHackflightFlightManager
