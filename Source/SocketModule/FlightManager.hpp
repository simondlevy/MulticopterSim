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

            // First output value is time
            _telemetry[0] = time;

            // Next output values are state
            for (uint8_t k=0; k<12; ++k) {
                _telemetry[k+1] = _dynamics->x(k);
            }

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor values from server
            _motorServer->receiveData(values, 8 * _actuatorCount);

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
