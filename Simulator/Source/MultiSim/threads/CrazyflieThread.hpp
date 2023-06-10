/*
 * Socket-based flight-management class for MultiSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Thread.hpp"

#include "../sockets/TcpServerSocket.hpp"

class FCrazyflieThread : public FVehicleThread {

    private: 

        // Time : State
        double _telemetry[13] = {};

        // Socket comms
        TcpServerSocket * _telemServer = NULL;

        // Guards socket comms
        bool _connected = false;

        void doComms(
                const double time,
                const Dynamics * dynamics,
                float * motors,
                float * newjoyvals)
        {
            // XXX should get these from client
            (void)newjoyvals;

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

            // Send _telemetry data
            _telemServer->sendData(_telemetry, sizeof(_telemetry));

            // XXX should get these from Crazyflie firmware
            motors[0] = 0.6;
            motors[1] = 0.6;
            motors[2] = 0.6;
            motors[3] = 0.6;
        }

    protected:

        virtual void getMotors(
                const double time,
                const float * joyvals,
                const Dynamics * dynamics,
                float * motors,
                const uint8_t motorCount) override
        {
            (void)joyvals;
            (void)motorCount;

            if (_telemServer) {

                if (_connected) {

                    float newjoyvals[4];
                    doComms(time, dynamics, motors, newjoyvals);
                }

                else {

                    if (_telemServer->acceptConnection()) {

                        _connected = true;
                    }
                }
            }
        }

    public:

        // Constructor, called main thread
        FCrazyflieThread(
                Dynamics * dynamics,
                const char * host = "127.0.0.1",
                const short port = 5000)
            : FVehicleThread(dynamics)
        {
            // Use non-blocking socket
            _telemServer = new TcpServerSocket(host, port, true);
        }

        ~FCrazyflieThread(void) 
        {
            if (_telemServer) {
                _telemServer->closeConnection();
            }
        }

}; // class FCrazyflieThread
