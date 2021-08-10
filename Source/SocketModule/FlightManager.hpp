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

#include "sockets/UdpClientSocket.hpp"
#include "sockets/UdpServerSocket.hpp"

#include "SocketCamera.hpp"

class FSocketFlightManager : public FFlightManager {

    private:

        // Socket comms
        UdpClientSocket * _telemClient = NULL;
        UdpServerSocket * _motorServer = NULL;

        // Joystick (RC transmitter, game controller) or keypad
        GameInput * _gameInput = NULL;

	    // Time : State : Demands
        double _telemetry[17] = {};

        // Helps synchronize threads
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

            _telemClient = new UdpClientSocket(host, telemPort);
            _motorServer = new UdpServerSocket(motorPort);

            _running = true;
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

        virtual void getActuators(const double time,
                double * actuatorValues) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!(_telemClient && _motorServer && _running)) {
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

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor actuatorValues from server
            _motorServer->receiveData(actuatorValues, 8 * _actuatorCount);

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
            _gameInput->getKeypad(&_telemetry[13]);
        }

}; // FSocketFlightManager
