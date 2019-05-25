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

    friend class AVehiclePawn;

    private:

        // Constants specified/computed in constructor
        uint8_t _motorCount = 0;
        double  _deltaT = 0;

        // Start-time offset so timing begins at zero
        double _startTime = 0;

        // Kinematics
        double   _position[3] = {0};
        double   _rotation[3] = {0};
        double * _motorvals = NULL; 
        
        // For computing _deltaT
        double   _previousTime = 0;

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
        FFlightManager(uint8_t motorCount, double initialPosition[3], double initialRotation[3], uint16_t updateFrequency=1000) : FThreadedWorker()
        {
            // Allocate array for motor values
            _motorvals = new double[motorCount];

            // Create vehicle dynamics via factory method
            _dynamics = MultirotorDynamics::create();

            // Initialize dynamics with initial pose
            _dynamics->init(initialPosition, initialRotation);

            // Initialize kinematics
            for (uint8_t j=0; j<3; ++j) {
                _position[j] = initialPosition[j];
                _rotation[j] = initialRotation[j];
            }

            // Constants
            _deltaT = 1. / updateFrequency;
            _motorCount = motorCount;

            // For periodic update
            _startTime = FPlatformTime::Seconds();
            _previousTime = 0;
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

                // Get vehicle state from dynamics.  We keep pose (position, rotation) in memory for use  in
                // getKinematics() method
                double angularVel[3]   = {0}; // body frame
                double inertialAcc[3]  = {0}; // inertial frame
                double intertialVel[3] = {0}; // inertial frame
                _dynamics->getState(angularVel, inertialAcc, _rotation, intertialVel, _position);

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

        virtual void getGimbal(float & roll, float &pitch) { roll = 0; pitch = 0; }

    public:

        ~FFlightManager(void)
        {
            delete _dynamics;
            delete _motorvals;
        }


        // Called by VehiclePawn::Tick() method to get current display kinematics
        // (position, rotation) and propeller animation/sound (motorvals)
        void getKinematics(double position[3], double rotation[3], double * motorvals)
        {
            for (uint8_t j=0; j<_motorCount; ++j) {
                motorvals[j] = _motorvals[j];
            }

            for (uint8_t k=0; k<3; ++k) {
                position[k] = _position[k];
                rotation[k] = _rotation[k];
            }
        }
        // Factory method implemented by your subclass
        static FFlightManager * createFlightManager(double initialPosition[3], double initialRotation[3]);
};
