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

#include "sockets/UdpClientSocket.hpp"
#include "sockets/UdpServerSocket.hpp"

class FSocketFlightManager : public FFlightManager {

    private:

        // Socket comms
        UdpClientSocket * _telemClient = NULL;
        UdpServerSocket * _motorServer = NULL;

	    // Time : State : Demands
        double _telemetry[17] = {};

        // Guards socket comms
        bool _connected = false;

        // Joystick / game controller / RC transmitter
        IJoystick * _joystick;

    public:

        FSocketFlightManager(APawn * pawn,
                Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001) : 
            FFlightManager(dynamics)
        {
            _telemClient = new UdpClientSocket(host, telemPort);
            _motorServer = new UdpServerSocket(motorPort);

            _joystick = new IJoystick();

            _connected = true;
        }
		
        ~FSocketFlightManager()
        {
            // Send a bogus time value to tell remote server we're done
            _telemetry[0] = -1;
            if (_telemClient) {
                _telemClient->sendData(_telemetry, sizeof(_telemetry));
            }

            // Close sockets
            UdpClientSocket::free(_telemClient);
            UdpServerSocket::free(_motorServer);
        }

        virtual void getMotors(const double time, double * values) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!(_telemClient && _motorServer && _connected)) {
                return;
            }

            double joyvals[10] = {};

            _joystick->poll(joyvals);

            // First output value is time
            _telemetry[0] = time;

            // Next output values are state
            _telemetry[1] = _dynamics->vstate.x;
            _telemetry[2] = _dynamics->vstate.dx;
            _telemetry[3] = _dynamics->vstate.y;
            _telemetry[4] = _dynamics->vstate.dy;
            _telemetry[5] = _dynamics->vstate.z;
            _telemetry[6] = _dynamics->vstate.dz;
            _telemetry[7] = _dynamics->vstate.phi;
            _telemetry[8] = _dynamics->vstate.dphi;
            _telemetry[9] = _dynamics->vstate.theta;
            _telemetry[10] = _dynamics->vstate.dtheta;
            _telemetry[11] = _dynamics->vstate.psi;
            _telemetry[12] = _dynamics->vstate.dpsi;

            // Remaining output values are stick demands
            _telemetry[13] = joyvals[0];
            _telemetry[14] = joyvals[1];
            _telemetry[15] = joyvals[2];
            _telemetry[16] = joyvals[3];

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor values from server
            _motorServer->receiveData(values, 8 * _actuatorCount);

            static uint64_t _foo;
            debugline("%10d: %f", _foo++, values[0]);

            // Server sends a -1 to halt
            if (values[0] == -1) {
				values[0] = 0;
				_connected = false;
				return;
			}
        }

        void tick(void)
        {
        }

}; // FSocketFlightManager
