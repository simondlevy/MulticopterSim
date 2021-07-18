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
#include "../MainModule/Joystick.h"
#include "sockets/TwoWayUdp.hpp"
#include "SocketCamera.hpp"

class FSocketFlightManager : public FFlightManager {

    private:

        TwoWayUdp * _twoWayUdp = NULL;

        // Time : State : Demands
        double _output[17] = {};

        bool _running = false;

    public:

        FSocketFlightManager(Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001) : 
            FFlightManager(dynamics)
        {
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

            // First output value is time
            _output[0] = time;

            // Next output values are state
            for (uint8_t k=0; k<12; ++k) {
                _output[k+1] = _dynamics->x(k);
            }

            // Last output are open-loop controller demands

			_twoWayUdp->send(_output, sizeof(_output));

			// Get motor actuatorValues from control program
			_twoWayUdp->receive(actuatorValues, 8 * _actuatorCount);

			// Control program sends a -1 to halt
			if (actuatorValues[0] == -1) {
				actuatorValues[0] = 0;
				_running = false;
				return;
			}
        }

}; // FSocketFlightManager
