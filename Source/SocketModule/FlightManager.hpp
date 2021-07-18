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
#include "sockets/TwoWayUdp.hpp"
#include "SocketCamera.hpp"

class FSocketFlightManager : public FFlightManager {

    private:

        // Socket comms
        TwoWayUdp * _twoWayUdp = NULL;

        // Joystick (RC transmitter, game controller) or keypad
        GameInput * _gameInput = NULL;

        static const uint8_t INPUT_SIZE = 4;

        // Values from joystick/keypad
        float _inputValues[INPUT_SIZE] = {};

	    // Time : State : Demands
        double _output[13+INPUT_SIZE] = {};

        bool _running = false;

    public:

        FSocketFlightManager(APawn * pawn,
                Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001) : 
            FFlightManager(dynamics)
        {
            _gameInput = new GameInput(pawn);

            _twoWayUdp = new TwoWayUdp(host, telemPort, motorPort);

            _running = true;
        }
		
        ~FSocketFlightManager()
        {
            // Send a bogus time value to tell remote server we're done
            _output[0] = -1;
			_twoWayUdp->send(_output, sizeof(_output));

            delete _twoWayUdp;
        }

        virtual void getActuators(const double time, double * actuatorValues) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control program halts
            if (!_twoWayUdp|| !_running) {
                return;
            }

            // Get demands from joystick
		    _gameInput->getJoystick(_inputValues);

            // First output value is time
            _output[0] = time;

            // Next output values are state
            for (uint8_t k=0; k<12; ++k) {
                _output[k+1] = _dynamics->x(k);
            }

            // Last output values are open-loop controller demands
            for (uint8_t k=0; k<INPUT_SIZE; ++k) {
                _output[k+13] = _inputValues[k];
            }

            // Send output values to server
			_twoWayUdp->send(_output, sizeof(_output));

			// Get motor actuatorValues from server
			_twoWayUdp->receive(actuatorValues, 8 * _actuatorCount);

			// Server sends a -1 to halt
			if (actuatorValues[0] == -1) {
				actuatorValues[0] = 0;
				_running = false;
				return;
			}
        }

        void tick(void)
        {
            // Get demands from keypad
            _gameInput->getKeypad(_inputValues);
        }

}; // FSocketFlightManager
