/*
 * Socket-based flight-management class for MultiSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Thread.hpp"

// XXX Fake up flight control with Hackflight for now
#include "hackflight.hpp"

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

        HackflightForSim hf;

        double _pose[6];
        float _sticks[4];
        double _cfjoyvals[4];

    protected:

        virtual void getMotors(

                const Dynamics * dynamics_in,
                float * motors_out,

                const double time,
                const float * joyvals,
                const uint8_t motorCount) override
        {

            (void)time;
            (void)joyvals;
            (void)motorCount;

            if (_server) {

                if (_connected) {

                    static bool was_connected;
                    if (!was_connected) {
                        printf("Connected\n");
                        fflush(stdout);
                        was_connected = true;
                    }

                    /*
                    _pose[0] = dynamics_in->vstate.x;
                    _pose[1] = dynamics_in->vstate.y;
                    _pose[2] = -dynamics_in->vstate.z;  // NED => ENU
                    _pose[3] = dynamics_in->vstate.phi;
                    _pose[4] = dynamics_in->vstate.theta;
                    _pose[5] = dynamics_in->vstate.psi;
                    */

                    _pose[0] = 1;
                    _pose[1] = 2;
                    _pose[2] = 3;
                    _pose[3] = 4;
                    _pose[4] = 5;
                    _pose[5] = 6;

                    _server->sendData((void *)_pose, sizeof(_pose));

                    _server->receiveData(_cfjoyvals, sizeof(_cfjoyvals));

                    _sticks[0] = (float)_cfjoyvals[0] / 80;
                    _sticks[1] = (float)_cfjoyvals[1] / 31;
                    _sticks[2] = (float)_cfjoyvals[2] / 31;
                    _sticks[3] = (float)_cfjoyvals[3] / 200;

                    // Run flight controller to get motor values
                    float motors[4] = {};
                    hf.step(
                            _count++ * DELTA_T,
                            _sticks,
                            dynamics_in,
                            motors_out,
                            4);
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
                const short port = 5000)
            : FVehicleThread(dynamics)
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
                        "x=%f  y=%+3.3f  z=%+3.3f",
                        _pose[0], _pose[1], _pose[2]);
            }
            else {
                mysprintf(message, "Waiting for client ...");
            }
        }


}; // class FCrazyflieThread
