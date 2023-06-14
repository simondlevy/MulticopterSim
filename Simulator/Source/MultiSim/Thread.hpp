/*
 * Abstract hreaded flight-management class for MultiSim
 *
 * Gets instantiated in Vehicle::beginPlay()
 *
 * Subclasses should implement getActuators()
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include "Dynamics.hpp"
#include "Utils.hpp"

#include "Runtime/Core/Public/HAL/Runnable.h"

class FVehicleThread : public FRunnable {

    private:

        FRunnableThread * _thread = NULL;

        // Flags set by begin/end play
        bool _running = false;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // For benchmarking
        uint32_t _dynamicsCount;
        uint32_t _pidCount;

        // Relates dynamics update to PID update
        uint32_t _controllerPeriod;

        // Set by controller; returned for animation
        float _actuatorValues[100] = {}; 

        uint8_t _actuatorCount = 0;

        Dynamics * _dynamics = NULL;

    protected:

        virtual void getActuators(
                const Dynamics * dynamics,
                const double timeSec,
                const uint8_t motorCount,
                float * motors) = 0;

    public:

        // Constructor, called main thread
        FVehicleThread(
                Dynamics * dynamics,
                const uint32_t controllerPeriod=100)
        {
            _thread =
                FRunnableThread::Create(
                        this, TEXT("FThreadedManager"), 0, TPri_BelowNormal);

            _controllerPeriod = controllerPeriod;

            _startTime = FPlatformTime::Seconds();

            _pidCount = 0;
            _dynamicsCount = 0;

            _actuatorCount = dynamics->actuatorCount();

            _dynamics = dynamics;
        }

        ~FVehicleThread(void)
        {
           delete _thread;
        }

        // Called by Vehicle::tick()
        virtual void getMessage(char * message)
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
                if (_controllerClock == _controllerPeriod) {
                    getActuators(
                            _dynamics, 
                            currentTime,
                            _actuatorCount,
                            _actuatorValues);

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
