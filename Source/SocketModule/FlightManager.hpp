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
#include "sockets/TwoWayUdp.hpp"
#include "SocketCamera.hpp"

class FSocketFlightManager : public FFlightManager {

    private:

        TwoWayUdp * _twoWayUdp = NULL;

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
            double telemetry[10] = {0};
            telemetry[0] = -1;
			_twoWayUdp->send(telemetry, sizeof(telemetry));

            delete _twoWayUdp;
        }

        virtual void getActuators(const double time, double * values) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control program halts
            if (!_twoWayUdp|| !_running) {
                return;
            }

            double telemetry[13] = {};

            telemetry[0] = time;

            for (uint8_t k=0; k<12; ++k) {
                telemetry[k+1] = _dynamics->x(k);
            }

			_twoWayUdp->send(telemetry, sizeof(telemetry));

			// Get motor values from control program
			_twoWayUdp->receive(values, 8 * _actuatorCount);

			// Control program sends a -1 to halt
			if (values[0] == -1) {
				values[0] = 0;
				_running = false;
				return;
			}
        }

}; // FSocketFlightManager
