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
        // Socket comms
        TcpServerSocket * _telemServer = NULL;

        // Guards socket comms
        bool _connected = false;

    protected:

        virtual void getMotors(
                const double time,
                const float * joyvals,
                const Dynamics * dynamics,
                float * motorValues,
                const uint8_t motorCount) override
        {
            if (_telemServer) {

                if (!_connected) {

                    if (_telemServer->acceptConnection()) {

                        _connected = true;
                    }

                    else {
                        static long _count;
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
        }

        ~FNewRemoteThread(void) 
        {
        }

}; // class FNewRemoteThread
