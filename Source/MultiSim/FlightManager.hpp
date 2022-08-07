/*
 * Threaded flight-management class for MultiSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include "Dynamics.hpp"
#include "Joystick.h"
#include "Utils.hpp"

#include "sockets/UdpClientSocket.hpp"
#include "sockets/UdpServerSocket.hpp"

#include "Runtime/Core/Public/HAL/Runnable.h"

class FFlightManager : public FRunnable {

    private:

        FRunnableThread * _thread = NULL;

        bool _running = false;

        // Socket comms
        UdpClientSocket * _telemClient = NULL;
        UdpServerSocket * _motorServer = NULL;

	    // Time : State : Demands
        double _telemetry[17] = {};

        // Guards socket comms
        bool _connected = false;

        // Joystick / game controller / RC transmitter
        IJoystick * _joystick;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // For FPS reporting
        uint32_t _count;

        double _actuatorValues[100] = {}; 

        // For computing deltaT
        double   _previousTime = 0;

        /**
         * Flight-control method running repeatedly on its own thread.  
         * Override this method to implement your own flight controller.
         *
         * @param time current time in seconds (input)
         * @param values actuator values returned by your controller (output)
         *
         */

        uint8_t _actuatorCount = 0;

        Dynamics * _dynamics = NULL;

        uint32_t getFps(void)
        {
            return (uint32_t)(_count/(FPlatformTime::Seconds()-_startTime));
        }

        void getMotors(const double time, double * values)
        {
            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!(_telemClient && _motorServer && _connected)) {
                return;
            }

            double joyvals[10] = {};

            _joystick->poll(joyvals);

            // First output value is time
            _telemetry[0] = time;

            // Next output values are state
            _telemetry[1] = _dynamics->vstate.x;
            _telemetry[2] = _dynamics->vstate.dx;
            _telemetry[3] = _dynamics->vstate.y;
            _telemetry[4] = _dynamics->vstate.dy;
            _telemetry[5] = _dynamics->vstate.z;
            _telemetry[6] = _dynamics->vstate.dz;
            _telemetry[7] = _dynamics->vstate.phi;
            _telemetry[8] = _dynamics->vstate.dphi;
            _telemetry[9] = _dynamics->vstate.theta;
            _telemetry[10] = _dynamics->vstate.dtheta;
            _telemetry[11] = _dynamics->vstate.psi;
            _telemetry[12] = _dynamics->vstate.dpsi;

            // Remaining output values are stick demands
            _telemetry[13] = joyvals[0];
            _telemetry[14] = joyvals[1];
            _telemetry[15] = joyvals[2];
            _telemetry[16] = joyvals[3];

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor values from server
            values[0] = 0;
            _motorServer->receiveData(values, 8 * _actuatorCount);

            static uint64_t _foo;
            debugline("%10d: %f", _foo++, values[0]);

            // Server sends a -1 to halt
            if (values[0] == -1) {
				values[0] = 0;
				_connected = false;
				return;
			}
        }

    public:

        // Constructor, called main thread
        FFlightManager(APawn * pawn,
                Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001)
        {
            _thread =
                FRunnableThread::Create(this, TEXT("FThreadedManage"), 0, TPri_BelowNormal); 
            _startTime = FPlatformTime::Seconds();
            _count = 0;

            // Constant
            _actuatorCount = dynamics->actuatorCount();

            _dynamics = dynamics;

            // For periodic update
            _previousTime = 0;

            _telemClient = new UdpClientSocket(host, telemPort);
            _motorServer = new UdpServerSocket(motorPort);

            _joystick = new IJoystick();

            _connected = true;
        }

        ~FFlightManager(void)
        {
            // Send a bogus time value to tell remote server we're done
            _telemetry[0] = -1;
            if (_telemClient) {
                _telemClient->sendData(_telemetry, sizeof(_telemetry));
            }

            // Close sockets
            UdpClientSocket::free(_telemClient);
            UdpServerSocket::free(_motorServer);

           delete _thread;
        }

        // Called by VehiclePawn::Tick() method to get actuator value for
        // animation and sound
        double actuatorValue(uint8_t index)
        {
            return _actuatorValues[index];
        }

        uint32_t getCount(void)
        {
            return _count;
        }

        static void stopThread(FFlightManager ** worker)
        {
            if (*worker) {
                (*worker)->Stop();
                delete *worker;
            }

            *worker = NULL;
        }

        // FRunnable interface.

        virtual bool Init() override
        {
            _running = false;

            return FRunnable::Init();
        }

        virtual uint32_t Run() override
        {
            // Initial wait before starting
            FPlatformProcess::Sleep(0.5);

            _running = true;

            while (_running) {

                // Get a high-fidelity current time value from the OS
                double currentTime = FPlatformTime::Seconds() - _startTime;

                // Update dynamics
                _dynamics->update(_actuatorValues, currentTime - _previousTime);

                // PID controller: update the flight manager (e.g.,
                // HackflightManager) with the dynamics state, getting back the
                // actuator values
                this->getMotors(currentTime, _actuatorValues);

                // Track previous time for deltaT
                _previousTime = currentTime;

                // Increment count for FPS reporting
                _count++;
            }

            return 0;
        }

        virtual void Stop() override
        {
            _running = false;

            // Final wait after stopping
            FPlatformProcess::Sleep(0.03);

            FRunnable::Stop();
        }

}; // class FFlightManager
