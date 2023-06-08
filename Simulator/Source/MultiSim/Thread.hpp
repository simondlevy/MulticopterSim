/*
 * Abstract hreaded flight-management class for MultiSim
 *
 * Gets instantiated in Vehicle::beginPlay()
 *
 * Subclasses should implement getMotors()
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include "Dynamics.hpp"
#include "Joystick.h"
#include "Utils.hpp"

#include "Runtime/Core/Public/HAL/Runnable.h"

class FVehicleThread : public FRunnable {

    private:

        FRunnableThread * _thread = NULL;

        bool _running = false;

        // Joystick / game controller / RC transmitter
        IJoystick * _joystick;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // For benchmarking
        uint32_t _dynamics_count;
        uint32_t _pid_count;

        // Relates dynamics update to PID update
        uint32_t _pid_period;

        float _actuatorValues[100] = {}; 

        // For computing deltaT
        double _previousTime = 0;

        uint8_t _actuatorCount = 0;

        Dynamics * _dynamics = NULL;

    protected:

        char _message[100];

        virtual void getMotors(
                const double time,
                const float * joyvals,
                const Dynamics * dynamics,
                float * motorValues,
                const uint8_t motorCount) = 0;

    public:

        // Constructor, called main thread
        FVehicleThread(Dynamics * dynamics, const uint32_t pidPeriod=100)
        {
            _thread =
                FRunnableThread::Create(
                        this, TEXT("FThreadedManager"), 0, TPri_BelowNormal);

            _pid_period = pidPeriod;

            _startTime = FPlatformTime::Seconds();

            _pid_count = 0;
            _dynamics_count = 0;

            _actuatorCount = dynamics->actuatorCount();

            _dynamics = dynamics;

            _previousTime = 0;

            _joystick = new IJoystick();
        }

        ~FVehicleThread(void)
        {
           delete _thread;
        }

        virtual const char * getMessage(void)
        {
            /*
            auto dt = FPlatformTime::Seconds()-_startTime;

            sprintf_s(_message,
                    "Dynamics=%3.3e Hz  PID=%3.3e",
                    _dynamics_count/dt,
                    _pid_count/dt);
                    */

            return _message;
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

                // Get a high-fidelity current time value from the OS
                double currentTime = FPlatformTime::Seconds() - _startTime;

                // Update dynamics
                _dynamics->update(
                        _actuatorValues, currentTime - _previousTime);

                // PID controller: periodically update the vehicle thread with
                // the dynamics state, getting back the actuator values
                static uint32_t _pid_clock;
                _pid_clock ++;
                if (_pid_clock ==  _pid_period) {
                    float joyvals[10] = {};
                    _joystick->poll(joyvals);
                    this->getMotors(
                            currentTime,
                            joyvals, 
                            _dynamics,
                            _actuatorValues,
                            _actuatorCount);

                    _pid_clock = 0;

                    // Increment count for FPS reporting
                    _pid_count++;
                }

                _dynamics_count++;

                // Track previous time for deltaT
                _previousTime = currentTime;
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
