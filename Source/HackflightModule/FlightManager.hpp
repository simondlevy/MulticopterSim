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
#include "../MainModule/GameInput.hpp"

class FHackflightFlightManager : public FFlightManager {

    private:

        // Joystick (RC transmitter, game controller) or keypad
        GameInput * _gameInput = NULL;

	    // Time : State : Demands
        double _telemetry[17] = {};

        // Guards socket comms
        bool _ready = false;

    public:

        FHackflightFlightManager(APawn * pawn, Dynamics * dynamics)
            : FFlightManager(dynamics)
        {
            _gameInput = new GameInput(pawn);

            _ready = true;
        }
		
        ~FHackflightFlightManager()
        {
            // Send a bogus time value to tell remote server we're done
            _telemetry[0] = -1;
        }

        virtual void getActuators(const double time, double * values) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!_ready) {
                return;
            }

            // First output value is time
            _telemetry[0] = time;

            // Next output values are state
            for (uint8_t k=0; k<12; ++k) {
                _telemetry[k+1] = _dynamics->x(k);
            }

            // Remaining values are stick demands
            _gameInput->getJoystick(&_telemetry[13]);
        }

        void tick(void)
        {
            // Get demands from keypad
            _gameInput->getKeypad(&_telemetry[13]);
        }

}; // FHackflightFlightManager
