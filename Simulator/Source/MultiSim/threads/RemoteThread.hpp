/*
 * Socket-based flight-management class for MultiSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Thread.hpp"

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
                const double time, const double * joyvals) override
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!(_telemClient && _motorServer && _connected)) {
                return;
            }

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
            _actuatorValues[0] = 0;
            _motorServer->receiveData(_actuatorValues, 8 * _actuatorCount);

            // Server sends a -1 to halt
            if (_actuatorValues[0] == -1) {
                _actuatorValues[0] = 0;
                _connected = false;
                return;
            }
        }

    public:

        // Constructor, called main thread
        FRemoteThread(APawn * pawn,
                Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001)

            : FVehicleThread(pawn, dynamics)
        {
            _telemClient = new UdpClientSocket(host, telemPort);
            _motorServer = new UdpServerSocket(motorPort);

            _connected = true;
        }

        ~FRemoteThread(void) 
            : ~FVehicleThread()
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
