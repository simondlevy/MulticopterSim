/*
 * Abstract, threaded flight-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "dynamics/MultirotorDynamics.hpp"
#include "ThreadedManager.hpp"

class FFlightManager : public FThreadedManager {

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
        
    protected:

        uint8_t _motorCount = 0;

        MultirotorDynamics * _dynamics = NULL;

        MultirotorDynamics::state_t _state = {};

        // Constructor, called main thread
        FFlightManager(MultirotorDynamics * dynamics) 
            : FThreadedManager()
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
            _dynamics->setMotors(_motorvals);

            // Update dynamics
            _dynamics->update(deltaT);

            // Get new vehicle state
            _state = _dynamics->getState();

            // PID controller: update the flight manager (e.g., HackflightManager) with
            // the dynamics state, getting back the motor values
            this->getMotors(currentTime, _state, _motorvals);

            debug("t=%3.3f  qw=%+3.3f", currentTime, _state.quaternion[0]);

            // Track previous time for deltaT
            _previousTime = currentTime;
        }

        // Supports subclasses that might need direct access to dynamics state vector
        double * getVehicleStateVector(void)
        {
            return _dynamics->getStateVector();
        }

    public:

        static const uint8_t MAX_MOTORS = 16;

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
