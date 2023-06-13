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
                const Dynamics * dynamics_in,
                float * motors_out,

                const double time,
                const float * joyvals,
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
            _telemetry[1] = dynamics_in->vstate.x;
            _telemetry[2] = dynamics_in->vstate.dx;
            _telemetry[3] = dynamics_in->vstate.y;
            _telemetry[4] = dynamics_in->vstate.dy;
            _telemetry[5] = dynamics_in->vstate.z;
            _telemetry[6] = dynamics_in->vstate.dz;
            _telemetry[7] = dynamics_in->vstate.phi;
            _telemetry[8] = dynamics_in->vstate.dphi;
            _telemetry[9] = dynamics_in->vstate.theta;
            _telemetry[10] = dynamics_in->vstate.dtheta;
            _telemetry[11] = dynamics_in->vstate.psi;
            _telemetry[12] = dynamics_in->vstate.dpsi;

            // Remaining output values are stick demands
            _telemetry[13] = (double)joyvals[0];
            _telemetry[14] = (double)joyvals[1];
            _telemetry[15] = (double)joyvals[2];
            _telemetry[16] = (double)joyvals[3];

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor values from server
            _motorServer->receiveData(motors_out, sizeof(float) * motorCount);

            // Server sends a -1 to halt
            if (motors_out[0] == -1) {
                motors_out[0] = 0;
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
