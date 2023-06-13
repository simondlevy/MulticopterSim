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
// #define HACKFLIGHT

#ifdef HACKFLIGHT
#include "hackflight.hpp"
#endif

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

#ifdef HACKFLIGHT
        HackflightForSim _hf;
#endif

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

#ifdef HACKFLIGHT
                    _hf.getMotors(
                            time, joyvals, dynamics_in, motors_out, motorCount);
#else
                    for (auto k=0; k<motorCount; ++k) {
                        motors_out[k] = 0.6;
                    }
#endif
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
