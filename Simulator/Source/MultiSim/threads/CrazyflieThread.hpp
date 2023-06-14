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
        double _joyvals[4];

        char _host[100];
        uint16_t _port;

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

                    _pose[0] = dynamics->vstate.x;
                    _pose[1] = dynamics->vstate.y;
                    _pose[2] = -dynamics->vstate.z;  // NED => ENU
                    _pose[3] = dynamics->vstate.phi;
                    _pose[4] = dynamics->vstate.theta;
                    _pose[5] = dynamics->vstate.psi;

                    static double z;
                    _pose[2] = z;
                    z += .01;

                    _server->sendData((void *)_pose, sizeof(_pose));

                    _server->receiveData(_joyvals, sizeof(_joyvals));

                    _sticks[0] = (float)_joyvals[0] / 80;
                    _sticks[1] = (float)_joyvals[1] / 31;
                    _sticks[2] = (float)_joyvals[2] / 31;
                    _sticks[3] = (float)_joyvals[3] / 200;

                    actuatorValues[0] = 0.0;
                    actuatorValues[1] = 0.0;
                    actuatorValues[2] = 0.0;
                    actuatorValues[3] = 0.0;
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
                const uint32_t pidPeriod=100000)
            : FVehicleThread(dynamics, pidPeriod)
        {
            // Use non-blocking socket
            _server = new TcpServerSocket(host, port, true);

            _connected = false;

            strcpy(_host, host);
            _port = port;
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

                mysprintf(message, "z=%+3.3f", _pose[2]);
            }
            else {
                mysprintf(message, "Listening for client on %s:%d", 
                        _host, _port);
            }
        }


}; // class FCrazyflieThread
