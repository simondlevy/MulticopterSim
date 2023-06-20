/*
 * Socket-based flight-management class for MultiSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../LocalJoystickThread.hpp"

#include "../sockets/UdpClientSocket.hpp"
#include "../sockets/UdpServerSocket.hpp"

class FRemoteControlThread : public FLocalJoystickThread {

    private:

        // Time : State : Demands
        double _telemetry[17] = {};

        // Socket comms
        UdpClientSocket * _telemClient = NULL;
        UdpServerSocket * _motorServer = NULL;

        // Guards socket comms
        bool _connected = false;

    protected:

        virtual void getActuators(

                const Dynamics * dynamics,
                const double timeSec,
                const float * joyvals,
                const uint8_t actuatorCount,
                float * actuatorValues) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!(_telemClient && _motorServer && _connected)) {
                return;
            }

            // First output value is time
            _telemetry[0] = timeSec;

            // Next output values are state
            _telemetry[1] = dynamics->vstate.x;
            _telemetry[2] = dynamics->vstate.dx;
            _telemetry[3] = dynamics->vstate.y;
            _telemetry[4] = dynamics->vstate.dy;
            _telemetry[5] = dynamics->vstate.z;
            _telemetry[6] = dynamics->vstate.dz;
            _telemetry[7] = dynamics->vstate.phi;
            _telemetry[8] = dynamics->vstate.dphi;
            _telemetry[9] = dynamics->vstate.theta;
            _telemetry[10] = dynamics->vstate.dtheta;
            _telemetry[11] = dynamics->vstate.psi;
            _telemetry[12] = dynamics->vstate.dpsi;

            // Remaining output values are stick demands
            _telemetry[13] = (double)joyvals[0];
            _telemetry[14] = (double)joyvals[1];
            _telemetry[15] = (double)joyvals[2];
            _telemetry[16] = (double)joyvals[3];

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor values from server
            _motorServer->receiveData(
                    actuatorValues, sizeof(float) * actuatorCount);

            // Server sends a -1 to halt
            if (actuatorValues[0] == -1) {
                actuatorValues[0] = 0;
                _connected = false;
                return;
            }
        }

    public:

        // Constructor, called main thread
        FRemoteControlThread(
                Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001)

            : FLocalJoystickThread(dynamics)
        {
            _telemClient = new UdpClientSocket(host, telemPort);
            _motorServer = new UdpServerSocket(motorPort);

            _connected = true;
        }

        ~FRemoteControlThread(void) 
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

 }; // class FRemoteControlThread
