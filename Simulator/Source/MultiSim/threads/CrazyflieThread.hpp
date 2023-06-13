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

        // Time : State
        double _telemetry[13] = {};

        // Socket comms
        TcpServerSocket * _server = NULL;

        // Guards socket comms
        bool _connected = false;

        double _sticks[4] = {};

        HackflightForSim _hf;

    protected:

        virtual void getMotors(

                const Dynamics * dynamics_in,
                float * motors_out,

                const double time,
                const float * joyvals,
                const uint8_t motorCount) override
        {

            if (_server) {

                if (_connected) {

                    const double telemetry[] = {

                        // vehicle state
                        dynamics_in->vstate.x,
                        dynamics_in->vstate.y,
                        dynamics_in->vstate.z,
                        dynamics_in->vstate.phi,
                        dynamics_in->vstate.theta,
                        dynamics_in->vstate.psi
                    };

                    _server->sendData((void *)telemetry, sizeof(telemetry));

                    _server->receiveData(_sticks, sizeof(_sticks));

                    _hf.getMotors(
                            time, joyvals, dynamics_in, motors_out, motorCount);
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
