/*
 * Abstract, threaded flight-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"

#include "dynamics/MultirotorDynamics.hpp"

class FFlightManager : public FThreadedWorker {

    private:

        // Constants specified/computed in constructor
        uint8_t _motorCount = 0;
        double  _deltaT = 0;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // Kinematics
        MultirotorDynamics::pose_t _pose = {0};

        // Current motor values from PID controller
        double * _motorvals = NULL; 
        
        // For computing _deltaT
        double   _previousTime = 0;

        // Did we hit the ground?
        bool _crashed = false;

        // Implement for each subclass
        virtual void update(const double time, const MultirotorDynamics::state_t & state, double * motorvals)  = 0;

    protected:

        MultirotorDynamics * _dynamics;

        // Called once on main thread
        FFlightManager(uint8_t motorCount, FVector initialLocation, FRotator initialRotation, uint16_t updateFrequency=1000) : FThreadedWorker()
        {
            // Allocate array for motor values
            _motorvals = new double[motorCount];

            // Create vehicle dynamics via factory method
            _dynamics = MultirotorDynamics::create();

            // Convert ENU centimeters => NED meters
            _pose.location[0] =  initialLocation.X / 100;
            _pose.location[1] =  initialLocation.Y / 100;
            _pose.location[2] = -initialLocation.Z / 100;

            // Convert degrees => radians
            _pose.rotation[0] = FMath::DegreesToRadians(initialRotation.Roll);
            _pose.rotation[1] = FMath::DegreesToRadians(initialRotation.Pitch);
            _pose.rotation[2] = FMath::DegreesToRadians(initialRotation.Yaw);

            // Initialize dynamics with initial pose
            _dynamics->init(_pose);

            // Constants
            _deltaT = 1. / updateFrequency;
            _motorCount = motorCount;

            // For periodic update
            _startTime = FPlatformTime::Seconds();
            _previousTime = 0;

            // No crash yet
            _crashed = false;
        }
        //
        // Called repeatedly on worker thread to compute dynamics and run flight controller (PID)
        void performTask(void)
        {
            // Get a high-fidelity current time value from the OS
            double currentTime = FPlatformTime::Seconds() - _startTime;

            double deltaT = currentTime - _previousTime;

            if (deltaT >= _deltaT) {

                // Send current motor values to dynamics
                _dynamics->setMotors(_motorvals);

                // Update dynamics
                _dynamics->update(deltaT);

                // Get vehicle state from dynamics.  We keep pose (location, rotation) in memory for use  in
                // getKinematics() method
                MultirotorDynamics::state_t state = {0};
                _crashed = _dynamics->getState(state);

                // PID controller: update the flight manager (e.g., HackflightManager) with
                // the dynamics state
                this->update(currentTime, state, _motorvals);

                // Track previous time for deltaT
                _previousTime = currentTime;

                // Copy out kinematic part of state
                for (uint8_t k=0; k<3; ++k) {
                    _pose.location[k] = state.pose.location[k];
                    _pose.rotation[k] = state.pose.rotation[k];
                }
            }
        }

    public:

        ~FFlightManager(void)
        {
            delete _dynamics;
            delete _motorvals;
        }

        // Called by VehiclePawn::Tick() method to get current display pose
        // (location, rotation) and propeller animation/sound (motorvals)
        bool getKinematics(FVector & location, FRotator & rotation, float * motorvals)
        {
            // Get motor values for propeller animation / motor sound
            for (uint8_t j=0; j<_motorCount; ++j) {
                motorvals[j] = _motorvals[j];
            }

            // Convert NED meters => ENU centimeters
            location.X =  _pose.location[0] * 100; 
            location.Y =  _pose.location[1] * 100; 
            location.Z = -_pose.location[2] * 100; 

            // Convert radians to degrees
            rotation.Roll =  FMath::RadiansToDegrees(_pose.rotation[0]);
            rotation.Pitch = FMath::RadiansToDegrees(_pose.rotation[1]);
            rotation.Yaw =   FMath::RadiansToDegrees(_pose.rotation[2]);

            return _crashed;
        }

        // Implemented by subclass
        virtual void getGimbal(float & roll, float &pitch) { roll = 0; pitch = 0; }

        // Factory method implemented by your subclass
        static FFlightManager * createFlightManager(FVector initialLocation, FRotator initialRotation);
};
