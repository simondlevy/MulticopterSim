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

class FHackflightFlightManager : public FFlightManager {

    private:

        // PID controllers
		hf::RatePid _ratePid = hf::RatePid(0.225, 0.001875, 0.375);
		hf::YawPid _yawPid = hf::YawPid(1.0625, 0.005625);
        hf::LevelPid _levelPid = hf::LevelPid(0.20);
        hf::AltitudeHoldPid _altHoldPid;
        hf::PositionHoldPid _posHoldPid;

        // Joystick (RC transmitter, game controller) or keypad
        SimReceiver * _receiver = NULL;
        
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
