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

        // Time constant
        static constexpr double DELTA_T = 0.001;
        uint32_t _count;

        // Socket comms
        TcpServerSocket * _server = NULL;

        // Guards socket comms
        bool _connected = false;

        double _pose[6];
        float _sticks[4];
        double _cfjoyvals[4];

    protected:

        virtual void getActuators(
                const Dynamics * dynamics,
                const double timeSec,
                const uint8_t actuatorCount,
                float * actuatorValues) override
        {

            (void)timeSec;
            (void)actuatorCount;

            if (_server) {

                if (_connected) {

                    static bool was_connected;
                    if (!was_connected) {
                        printf("Connected\n");
                        fflush(stdout);
                        was_connected = true;
                    }

                    _pose[0] = dynamics->vstate.x;
                    _pose[1] = dynamics->vstate.y;
                    _pose[2] = -dynamics->vstate.z;  // NED => ENU
                    _pose[3] = dynamics->vstate.phi;
                    _pose[4] = dynamics->vstate.theta;
                    _pose[5] = dynamics->vstate.psi;

                    _server->sendData((void *)_pose, sizeof(_pose));

                    _server->receiveData(_cfjoyvals, sizeof(_cfjoyvals));

                    _sticks[0] = (float)_cfjoyvals[0] / 80;
                    _sticks[1] = (float)_cfjoyvals[1] / 31;
                    _sticks[2] = (float)_cfjoyvals[2] / 31;
                    _sticks[3] = (float)_cfjoyvals[3] / 200;

                    actuatorValues[0] = 0.6;
                    actuatorValues[1] = 0.6;
                    actuatorValues[2] = 0.6;
                    actuatorValues[3] = 0.6;
                }

                else {

                    if (_server->acceptConnection()) {

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
                const short port = 5000,
                const uint32_t pidPeriod=10000)
            : FVehicleThread(dynamics, pidPeriod)
        {
            // Use non-blocking socket
            _server = new TcpServerSocket(host, port, true);

            _connected = false;
        }

        ~FCrazyflieThread(void) 
        {
            if (_server) {
                _server->closeConnection();
                _connected = false;
            }

            _server = NULL;
        }

        virtual void getMessage(char * message) override 
        {
            if (_connected) {

                // FVehicleThread::getMessage(message);

                mysprintf(message, 
                        "x=%+3.3f  y=%+3.3f  z=%+3.3f",
                        _pose[0], _pose[1], _pose[2]);
            }
            else {
                mysprintf(message, "Waiting for client ...");
            }
        }


}; // class FCrazyflieThread
