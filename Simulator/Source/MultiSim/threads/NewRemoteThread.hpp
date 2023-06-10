/*
 * Local flight-management stub for MultiSim
 *
 * Spins all motors at 60%
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Thread.hpp"

#include "../sockets/TcpServerSocket.hpp"

class FNewRemoteThread : public FVehicleThread {

    private: 

        // Time : State : Demands
        double _telemetry[17] = {};

        // Socket comms
        TcpServerSocket * _telemServer = NULL;

        // Guards socket comms
        bool _connected = false;

        long _count;

        void doComms(
                const double time,
                const Dynamics * dynamics,
                const float * joyvals,
                float * motors,
                const uint8_t motorCount)
        {
            // First value is time
            _telemetry[0] = time;

            // Next 12 values are 12D state vector
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

            // Last four values are receiver demands
            _telemetry[13] = (double)joyvals[0];
            _telemetry[14] = (double)joyvals[1];
            _telemetry[15] = (double)joyvals[2];
            _telemetry[16] = (double)joyvals[3];

            // Send _telemetry data
            _telemServer->sendData(_telemetry, sizeof(_telemetry));

            // Get incoming motor values
            motors[0] = 0;
            _telemServer->receiveData(motors, sizeof(float) * motorCount);
        }

    protected:

        virtual void getMotors(
                const double time,
                const float * joyvals,
                const Dynamics * dynamics,
                float * motors,
                const uint8_t motorCount) override
        {
            if (_telemServer) {

                if (_connected) {

                    doComms(time, dynamics, joyvals, motors, motorCount);

                    sprintf_s(_message, "m1=%f", motors[0]);

                    /*
                    sprintf_s(_message,
                            "m1=%3.3f  m2=%3.3f  m3=%3.3f  m4=%3.3f",
                            motors[0], motors[1], motors[2], motors[3]);*/
                }

                else {

                    if (_telemServer->acceptConnection()) {

                        _connected = true;

                        _count = 0;
                    }

                    else {
                        sprintf_s(_message,
                                "Waiting on client: %ld", _count++);
                    }
                }
            }
        }

    public:

        // Constructor, called main thread
        FNewRemoteThread(
                Dynamics * dynamics,
                const char * host = "127.0.0.1",
                const short port = 5000,
                const uint32_t timeoutMsec=1)
            : FVehicleThread(dynamics)
        {
            // Use non-blocking socket
            _telemServer = new TcpServerSocket(host, port, true);

            _count = 0;
        }

        ~FNewRemoteThread(void) 
        {
            // Send a bogus time value to tell remote server we're done
            _telemetry[0] = -1;
            if (_telemServer) {
                _telemServer->sendData(_telemetry, sizeof(_telemetry));
                _telemServer->closeConnection();
            }
        }

}; // class FNewRemoteThread
