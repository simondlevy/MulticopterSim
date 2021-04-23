/*
   MulticopterSim FlightManager class implementation using UDP sockets 

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"
#include "../../Extras/sockets/TwoWayUdp.hpp"
#include "SocketCamera.hpp"

class FSocketFlightManager : public FFlightManager {

    private:

		const char * HOST = "127.0.0.1";
        const short MOTOR_PORT = 5000;
		const short TELEM_PORT = 5001;

        TwoWayUdp * _twoWayUdp = NULL;

        bool _running = false;

    public:

        FSocketFlightManager(Dynamics * dynamics) : 
            FFlightManager(dynamics)
        {
            _twoWayUdp = new TwoWayUdp(HOST, TELEM_PORT, MOTOR_PORT);

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

        virtual void getMotors(const double time, double * motorvals) override
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
			_twoWayUdp->receive(motorvals, 8 * _nmotors);

			// Control program sends a -1 to halt
			if (motorvals[0] == -1) {
				motorvals[0] = 0;
				_running = false;
				return;
			}
        }

}; // FSocketFlightManager
