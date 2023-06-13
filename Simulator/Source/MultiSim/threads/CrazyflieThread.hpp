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

                    const double pose[] = {

                        dynamics_in->vstate.x,
                        dynamics_in->vstate.y,
                        -dynamics_in->vstate.z,  // NED => ENU
                        dynamics_in->vstate.phi,
                        dynamics_in->vstate.theta,
                        dynamics_in->vstate.psi
                    };

                    _server->sendData((void *)pose, sizeof(pose));

                    double cfjoyvals[4] = {};
                    _server->receiveData(cfjoyvals, sizeof(cfjoyvals));

                    float sticks[4] = {
                        (float)cfjoyvals[0] / 80,
                        (float)cfjoyvals[1] / 31,
                        (float)cfjoyvals[2] / 31,
                        (float)cfjoyvals[3] / 200,
                    };

                    // Run flight controller to get motor values
                    float motors[4] = {};
                    hf.step(
                            _count++ * DELTA_T,
                            sticks,
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

                FVehicleThread::getMessage(message);
            }
            else {
                mysprintf(message, "Waiting for client ...");
            }
        }


}; // class FCrazyflieThread
