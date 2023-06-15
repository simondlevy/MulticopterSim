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

// Simple altitude-hold PI controller ///////////////////////////////////

// Throttle threshold for liftoff
static const float THROTTLE_THRESHOLD = 0.5;

// PI controller constants
static const double K_P = 4.0;
static const double K_I = 1.0;
static const double K_WINDUP_MAX = 1.0;
static const double Z_TARGET = 0.40;

static float constrain(const float val, const float min, const float max)
{
    return val < min ? min : val > max ? max : val;
}

static float getThrottle(const double z, const double dz)
{
    const auto error = (Z_TARGET - z) - dz;

    static double errorIntegral;

    errorIntegral = constrain(
            errorIntegral + error, -K_WINDUP_MAX, +K_WINDUP_MAX);

    return constrain(K_P * error + K_I * errorIntegral, 0, 1);
}


/////////////////////////////////////////////////////////////////////////

class FCrazyflieThread : public FVehicleThread {

    private: 

        // Socket comms
        TcpServerSocket * _server = NULL;

        // Guards socket comms
        bool _connected = false;

        double _pose[6];
        float _sticks[4];

        // For reporting
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

                    const auto vstate = dynamics->vstate;

                    _pose[0] = vstate.x;
                    _pose[1] = vstate.y;
                    _pose[2] = -vstate.z;  // NED => ENU
                    _pose[3] = vstate.phi;
                    _pose[4] = vstate.theta;
                    _pose[5] = vstate.psi;

                    _server->sendData((void *)_pose, sizeof(_pose));

                    double joyvals[4] = {};
                    _server->receiveData(joyvals, sizeof(joyvals));

                    _sticks[0] = (float)joyvals[0] / 80;
                    _sticks[1] = (float)joyvals[1] / 31;
                    _sticks[2] = (float)joyvals[2] / 31;
                    _sticks[3] = (float)joyvals[3] / 200;

                    static bool airborne;

                    if (_sticks[0] > THROTTLE_THRESHOLD) {
                        airborne = true;
                    }

                    const auto throttle = airborne ? 
                        getThrottle(-vstate.z, -vstate.dz) : 
                        0;

                    // Set all motors to same value for now
                    actuatorValues[0] = throttle;
                    actuatorValues[1] = throttle;
                    actuatorValues[2] = throttle;
                    actuatorValues[3] = throttle;
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
                const uint32_t controlPeriod=100000)

            : FVehicleThread(dynamics, controlPeriod)
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

                FVehicleThread::getMessage(message);
            }
            else {
                mysprintf(message, "Listening for client on %s:%d", 
                        _host, _port);
            }
        }


}; // class FCrazyflieThread
