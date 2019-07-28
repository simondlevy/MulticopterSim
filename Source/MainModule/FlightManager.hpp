/*
 * Abstract, threaded flight-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Core.h"
#include "Runnable.h"
#include "Debug.hpp"
#include "dynamics/MultirotorDynamics.hpp"
#include "ThreadedWorker.hpp"
#include <stdarg.h>

class FFlightManager : public FThreadedWorker {

    private:

        // Current motor values from PID controller
        double * _motorvals = NULL; 
        
        // For computing deltaT
        double   _previousTime = 0;

        // Flag for whether we've crashed and need to reset
        bool _crashed = false;

        /**
         * Flight-control method running repeatedly on its own thread.  
         * Override this method to implement your own flight controller.
         *
         * @param time current time in seconds (input)
         * @param state vehicle state (input)
         * @param motorvals motor values returned by your controller (output)
         *
         */
        virtual void getMotors(const double time, const MultirotorDynamics::state_t & state, double * motorvals)  = 0;
        
        // OSD
        void showStatus(const double time, const MultirotorDynamics::state_t & state)
        {
            /*
            const double * accel = state.bodyAccel;
            const double * gyro  = state.angularVel;
            const double * quat  = state.quaternion;
            const double * loc   = state.pose.location;

            debug(
                    "t: %4.1f"
                    " | a: %+6.6f %+6.6f %+6.6f"
                    " | g: %+6.6f %+6.6f %+6.6f"
                    " | q: %+6.6f %+6.6f %+6.6f %+6.6f"
                    " | p: %+6.6f %+6.6f %+6.6f\n", 
                    time, 
                    accel[0], accel[1], accel[2], 
                    gyro[0], gyro[1], gyro[2], 
                    quat[0], quat[1], quat[2], quat[3], 
                    loc[0], loc[1], loc[2]);
                    */

        }

    protected:

        uint8_t _motorCount = 0;

        MultirotorDynamics * _dynamics;

        MultirotorDynamics::state_t _state = {0};

        // Constructor, called main thread
        FFlightManager(MultirotorDynamics * dynamics) 
            : FThreadedWorker()
        {
            // Allocate array for motor values
            _motorvals = new double[dynamics->motorCount()];

            // Store dynamics for performTask()
            _dynamics = dynamics;

            // Constant
            _motorCount = dynamics->motorCount();

            // For periodic update
            _previousTime = 0;

            // No crash yet
            _crashed = false;
        }

        // Called repeatedly on worker thread to compute dynamics and run flight controller (PID)
        void performTask(double currentTime)
        {
            // Compute DeltaT
            double deltaT = currentTime - _previousTime;

            // Send current motor values to dynamics
            _dynamics->setMotors(_motorvals, deltaT);

            // Update dynamics
            _dynamics->update(deltaT);

            // Get new vehicle state
            _state = _dynamics->getState();

            // PID controller: update the flight manager (e.g., HackflightManager) with
            // the dynamics state, getting back the motor values
            this->getMotors(currentTime, _state, _motorvals);

            // Show status in OSD
            showStatus(currentTime, _state);

            // Track previous time for deltaT
            _previousTime = currentTime;
        }

    public:

        ~FFlightManager(void)
        {
        }

        // Called by VehiclePawn::Tick() method to propeller animation/sound (motorvals)
        void getMotorValues(float * motorvals)
        {
            // Get motor values for propeller animation / motor sound
            for (uint8_t j=0; j<_motorCount; ++j) {
                motorvals[j] = _motorvals[j];
            }
        }

}; // class FFlightManager
