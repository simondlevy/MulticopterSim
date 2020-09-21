/*
 * Abstract, threaded flight-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Dynamics.hpp"
#include "ThreadedManager.hpp"

class FFlightManager : public FThreadedManager {

    private:

        // Current motor values from PID controller
        double * _motorvals = NULL; 
        
        // For computing deltaT
        double   _previousTime = 0;

        bool _running = false;

        /**
         * Flight-control method running repeatedly on its own thread.  
         * Override this method to implement your own flight controller.
         *
         * @param time current time in seconds (input)
         * @param state vehicle state (input)
         * @param motorvals motor values returned by your controller (output)
         *
         */
        virtual void getMotors(const double time, const Dynamics::state_t & state, double * motorvals)  = 0;
        
    protected:

        uint8_t _motorCount = 0;

        Dynamics * _dynamics = NULL;

        Dynamics::state_t _state = {};

        // Constructor, called main thread
        FFlightManager(Dynamics * dynamics) 
            : FThreadedManager()
        {
            // Allocate array for motor values
            _motorvals = new double[dynamics->motorCount()]();

            // Store dynamics for performTask()
            _dynamics = dynamics;

            // Constant
            _motorCount = dynamics->motorCount();

            // For periodic update
            _previousTime = 0;

            _running = true;
        }

        // Called repeatedly on worker thread to compute dynamics and run flight controller (PID)
        void performTask(double currentTime)
        {
            if (!_running) return;

            // Compute time deltay in seconds
			double dt = currentTime - _previousTime;

            // Send current motor values and time delay to dynamics
            _dynamics->setMotors(_motorvals, dt);

            // Update dynamics
            _dynamics->update(dt);

            // Get new vehicle state
            _state = _dynamics->getState();

            // PID controller: update the flight manager (e.g., HackflightManager) with
            // the dynamics state, getting back the motor values
            this->getMotors(currentTime, _state, _motorvals);

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

        void stop(void)
        {
            _running = false;
        }

}; // class FFlightManager
