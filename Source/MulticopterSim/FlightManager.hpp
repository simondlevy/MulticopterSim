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
        double   _location[3] = {0};
        double   _rotation[3] = {0};
        double * _motorvals = NULL; 
        
        // For computing _deltaT
        double   _previousTime = 0;

        // Did we hit the ground?
        bool _crashed = false;

        // Useful conversion function
        void eulerToQuaternion(double eulerAngles[3], double quaternion[4])
        {
            // Convenient renaming
            double phi = eulerAngles[0] / 2;
            double the = eulerAngles[1] / 2;
            double psi = eulerAngles[2] / 2;

            // Pre-computation
            double cph = cos(phi);
            double cth = cos(the);
            double cps = cos(psi);
            double sph = sin(phi);
            double sth = sin(the);
            double sps = sin(psi);

            // Conversion
            quaternion[0] =  cph * cth * cps + sph * sth * sps;
            quaternion[1] =  cph * sth * sps - sph * cth * cps ;
            quaternion[2] = -cph * sth * cps - sph * cth * sps;
            quaternion[3] =  cph * cth * sps - sph * sth * cps;
        }


        // Implement for each subclass
        virtual void update(double time, double quat[4], double gyro[4], double * motorvals)  = 0;

    protected:

        MultirotorDynamics * _dynamics;

        // Called once on main thread
        FFlightManager(uint8_t motorCount, FVector initialLocation, FRotator initialRotation, uint16_t updateFrequency=1000) : FThreadedWorker()
        {
            // Allocate array for motor values
            _motorvals = new double[motorCount];

            // Create vehicle dynamics via factory method
            _dynamics = MultirotorDynamics::create();

            // Initialize kinematics
            
            _location[0] = initialLocation.X / 100;
            _location[1] = initialLocation.Y / 100;
            _location[2] = initialLocation.Z / 100;

            _rotation[0] = FMath::DegreesToRadians(initialRotation.Roll);
            _rotation[1] = FMath::DegreesToRadians(initialRotation.Pitch);
            _rotation[2] = FMath::DegreesToRadians(initialRotation.Yaw);

            // Initialize dynamics with initial pose
            _dynamics->init(_location, _rotation);

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

                //dbgprintf("%s", _dynamics->getMessage());

                // Get vehicle state from dynamics.  We keep pose (location, rotation) in memory for use  in
                // getKinematics() method
                double angularVel[3]   = {0}; // body frame
                double inertialAcc[3]  = {0}; // inertial frame
                double intertialVel[3] = {0}; // inertial frame
                _crashed = _dynamics->getState(angularVel, inertialAcc, _rotation, intertialVel, _location);

                // Convert Euler angles to quaternion
                double imuOrientationQuat[4]={0};
                eulerToQuaternion(_rotation, imuOrientationQuat);

                // PID controller: update the flight manager (e.g., HackflightManager) with
                // the quaternion and gyrometer, getting the resulting motor values
                update(currentTime, imuOrientationQuat, angularVel, _motorvals);

                // Track previous time for deltaT
                _previousTime = currentTime;
            }
        }

    public:

        ~FFlightManager(void)
        {
            delete _dynamics;
            delete _motorvals;
        }


        // Called by VehiclePawn::Tick() method to get current display kinematics
        // (location, rotation) and propeller animation/sound (motorvals)
        bool getKinematics(FVector & location, FRotator & rotation, double * motorvals)
        {
            for (uint8_t j=0; j<_motorCount; ++j) {
                motorvals[j] = _motorvals[j];
            }

            // Convert NED meters => ENU centimeters
            location.X =  _location[0] * 100; 
            location.Y =  _location[1] * 100; 
            location.Z = -_location[2] * 100; 

            // Convert radians to degrees
            rotation.Roll =  FMath::RadiansToDegrees(_rotation[0]);
            rotation.Pitch = FMath::RadiansToDegrees(_rotation[1]);
            rotation.Yaw =   FMath::RadiansToDegrees(_rotation[2]);

            return _crashed;
        }

        // Implemented by subclass
        virtual void getGimbal(float & roll, float &pitch) { roll = 0; pitch = 0; }

        // Factory method implemented by your subclass
        static FFlightManager * createFlightManager(FVector initialLocation, FRotator initialRotation);
};
