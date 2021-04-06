/*
   MulticopterSim FlightManager class implementation using UDP sockets 

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"
#include "../../Extras/sockets/TwoWayUdp.hpp"

class FSocketFlightManager : public FFlightManager {

    private:

		const char * HOST = "127.0.0.1";
        const short MOTOR_PORT = 5000;
		const short TELEM_PORT = 5001;

        TwoWayUdp * _twoWayUdp = NULL;

        uint8_t _nmotors = 0;

        bool _running = false;

        static void copy(double * dst, uint8_t pos, const double * src, uint8_t n)
        {
            for (uint8_t i=0; i<n; ++i) {
                dst[pos+i] = src[i];
            }
        }

    public:

        FSocketFlightManager(Dynamics * dynamics, int nmotors) : 
            FFlightManager(dynamics, nmotors)
        {
            _twoWayUdp = new TwoWayUdp(HOST, TELEM_PORT, MOTOR_PORT);

            _nmotors = nmotors;
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

            // Time Gyro, Quat, Location
            double telemetry[10] = {0};

            telemetry[0] = time;

            // XXX Replace state.* with _dynamics->x(Dynamics::STATE_*)
            // copy(telemetry, 1, state.angularVel, 3);
            // copy(telemetry, 4, state.bodyAccel, 3);
            // copy(telemetry, 7, state.pose.location, 3);

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
