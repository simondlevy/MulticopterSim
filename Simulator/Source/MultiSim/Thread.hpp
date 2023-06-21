/*
 * Abstract hreaded flight-management class for MultiSim
 *
 * Gets instantiated in Vehicle::beginPlay()
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include "../sockets/UdpClientSocket.hpp"
#include "../sockets/UdpServerSocket.hpp"

#include "../Joystick.h"

#include "Dynamics.hpp"
#include "Utils.hpp"

#include "Runtime/Core/Public/HAL/Runnable.h"

class FVehicleThread : public FRunnable {

    private:

        // Relates dynamics update to PID update
        static const uint32_t CONTROLLER_PERIOD = 100;

        // Time : State : Demands
        double _telemetry[17] = {};

        // Socket comms
        UdpClientSocket * _telemClient = NULL;
        UdpServerSocket * _motorServer = NULL;

        // Guards socket comms
        bool _connected = false;

        // Joystick / game controller / RC transmitter
        IJoystick * _joystick;

        FRunnableThread * _thread = NULL;

        // Flags set by begin/end play
        bool _running = false;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // For benchmarking
        uint32_t _dynamicsCount;
        uint32_t _pidCount;

        // Set by controller; returned for animation
        float _actuatorValues[100] = {}; 

        uint8_t _actuatorCount = 0;

        Dynamics * _dynamics = NULL;

        static double rad2deg(const double rad)
        {
            return (180 * rad / M_PI);
        }

        void getActuators(const double timeSec)
        {
            float joyvals[10] = {};
            _joystick->poll(joyvals);

            // Avoid null-pointer exceptions at startup, freeze after control
            // program halts
            if (!(_telemClient && _motorServer && _connected)) {
                return;
            }

            // First output value is time
            _telemetry[0] = timeSec;

            // Next output values are state
            _telemetry[1] = _dynamics->vstate.x;
            _telemetry[2] = _dynamics->vstate.dx;
            _telemetry[3] = _dynamics->vstate.y;
            _telemetry[4] = _dynamics->vstate.dy;
            _telemetry[5] = -_dynamics->vstate.z;        // NED => END
            _telemetry[6] = -_dynamics->vstate.dz;       // NED => ENU
            _telemetry[7] = rad2deg(_dynamics->vstate.phi);
            _telemetry[8] = rad2deg(_dynamics->vstate.dphi);
            _telemetry[9] = -rad2deg(_dynamics->vstate.theta);   // nose-up pos
            _telemetry[10] = -rad2deg(_dynamics->vstate.dtheta); // nose-up pos
            _telemetry[11] = rad2deg(_dynamics->vstate.psi);
            _telemetry[12] = rad2deg(_dynamics->vstate.dpsi);

            // Remaining output values are stick demands
            _telemetry[13] = ((double)joyvals[0] + 1) / 2;  // [-1,+1] => [0,1]
            _telemetry[14] = (double)joyvals[1];
            _telemetry[15] = (double)joyvals[2];
            _telemetry[16] = (double)joyvals[3];

            // Send telemetry values to server
            _telemClient->sendData(_telemetry, sizeof(_telemetry));

            // Get motor values from server
            _motorServer->receiveData(
                    _actuatorValues, sizeof(float) * _actuatorCount);

            // Server sends a -1 to halt
            if (_actuatorValues[0] == -1) {
                _actuatorValues[0] = 0;
                _connected = false;
                return;
            }
        }


    public:

        // Constructor, called main thread
        FVehicleThread(
                Dynamics * dynamics,
                const char * host="127.0.0.1",
                const short motorPort=5000,
                const short telemPort=5001)

        {
            _thread =
                FRunnableThread::Create(
                        this, TEXT("FThreadedManager"), 0, TPri_BelowNormal);

            _startTime = FPlatformTime::Seconds();

            _pidCount = 0;
            _dynamicsCount = 0;

            _actuatorCount = dynamics->actuatorCount();

            _dynamics = dynamics;

            _joystick = new IJoystick();

            _telemClient = new UdpClientSocket(host, telemPort);
            _motorServer = new UdpServerSocket(motorPort);

            _connected = true;
        }

        ~FVehicleThread(void)
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

        // Called by Vehicle::tick()
        void getMessage(char * message)
        {
            auto dt = FPlatformTime::Seconds()-_startTime;

            mysprintf(message,
                    "Dynamics=%3.3e Hz  Control=%3.3e Hz",
                    _dynamicsCount/dt,
                    _pidCount/dt);
        }

        // Called by VehiclePawn::Tick() method to get actuator value for
        // animation and sound
        float actuatorValue(uint8_t index)
        {
            return _actuatorValues[index];
        }

        static void stopThread(FVehicleThread ** worker)
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

                // For computing dynamics deltaT
                static double _previousDynamicsTime;

                // For computing dynamics deltaT
                static double _previousControllerTime;

                // Get a high-fidelity current time value from the OS
                double currentTime = FPlatformTime::Seconds() - _startTime;

                // Update dynamics
                _dynamics->update(_actuatorValues, 
                        currentTime - _previousDynamicsTime);

                // PID controller: periodically update the vehicle thread with
                // the dynamics state, getting back the actuator values
                static uint32_t _controllerClock;
                _controllerClock ++;
                if (_controllerClock == CONTROLLER_PERIOD) {

                    getActuators(currentTime);

                    _controllerClock = 0;

                    // Increment count for FPS reporting
                    _pidCount++;

                    _previousControllerTime = currentTime;
                }

                _dynamicsCount++;

                // Track previous time for deltaT
                _previousDynamicsTime = currentTime;
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

}; // class FVehicleThread
