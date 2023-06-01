/*
 * Socket-based flight-management class for MultiSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Thread.hpp"

#include "../sockets/UdpClientSocket.hpp"
#include "../sockets/UdpServerSocket.hpp"

class FRemoteThread : public FVehicleThread {

    private:

        // Time : State : Demands
        double _telemetry[17] = {};

        // Socket comms
        UdpClientSocket * _telemClient = NULL;
        UdpServerSocket * _motorServer = NULL;

        // Guards socket comms
        bool _connected = false;

    protected:

        virtual void getMotors(
                const double time,
                const double * joyvals,
                const Dynamics * dynamics,
                float * motors,
                const uint8_t motorCount) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!(_telemClient && _motorServer && _connected)) {
                return;
            }

            // First output value is time
            _telemetry[0] = time;

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
            _telemetry[13] = joyvals[0];
            _telemetry[14] = joyvals[1];
            _telemetry[15] = joyvals[2];
            _telemetry[16] = joyvals[3];

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor values from server
            motors[0] = 0;
            _motorServer->receiveData(motors, sizeof(float) * motorCount);

            // Server sends a -1 to halt
            if (motors[0] == -1) {
                motors[0] = 0;
                _connected = false;
                return;
            }
        }

    public:

        // Constructor, called main thread
        FRemoteThread(
                Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001)

            : FVehicleThread(dynamics)
        {
            _telemClient = new UdpClientSocket(host, telemPort);
            _motorServer = new UdpServerSocket(motorPort);

            _connected = true;
        }

        ~FRemoteThread(void) 
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

 }; // class FRemoteThread
