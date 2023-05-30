/*
 * Threaded flight-management class for MultiSim
 *
 * Gets instantiated in Vehicle::beginPlay()
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

        static const uint32_t PID_PERIOD = 100;

        FRunnableThread * _thread = NULL;

        bool _running = false;

        // Vehicle pawn
        APawn * _pawn = NULL;

        // Joystick / game controller / RC transmitter
        IJoystick * _joystick;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // For FPS reporting
        uint32_t _count;

        double _actuatorValues[100] = {}; 

        // For computing deltaT
        double _previousTime = 0;

        // For PID loop
        double _pidLoopTime = 0;

        /**
         * Flight-control method running repeatedly on its own thread.  
         *
         * @param time current time in seconds (input)
         * @param values actuator values returned by your controller (output)
         *
         */

        uint8_t _actuatorCount = 0;

        Dynamics * _dynamics = NULL;

    protected:

        virtual void getMotors(
                const double time,
                const double * joyvals,
                const Dynamics * dynamics,
                double *motors,
                const uint8_t motorCount) = 0;

    public:

        // Constructor, called main thread
        FVehicleThread(APawn * pawn, Dynamics * dynamics)
        {
            _pawn = pawn;

            _thread =
                FRunnableThread::Create(
                        this, TEXT("FThreadedManage"), 0, TPri_BelowNormal);

            _startTime = FPlatformTime::Seconds();

            _count = 0;

            _actuatorCount = dynamics->actuatorCount();

            _dynamics = dynamics;

            _previousTime = 0;
            _pidLoopTime = 0;

            _joystick = new IJoystick();
        }

        ~FVehicleThread(void)
        {
           delete _thread;
        }

        uint32_t getFps(void)
        {
            return (uint32_t)(_count/(FPlatformTime::Seconds()-_startTime));
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
                static uint32_t _pid_count;
                _pid_count ++;
                if (_pid_count ==  PID_PERIOD) {
                    double joyvals[10] = {};
                    _joystick->poll(joyvals);
                    this->getMotors(
                            currentTime,
                            joyvals, _dynamics,
                            _actuatorValues,
                            _actuatorCount);
                    _pid_count = 0;
                }

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

}; // class FVehicleThread
