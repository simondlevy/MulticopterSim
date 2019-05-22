/*
 * ThreadedWorker subclass for flight management (dynamics + PID control)
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "FlightManager.h"

// Called once on main thread
FFlightManager::FFlightManager(
        uint8_t motorCount,
        double initialPosition[3], 
        double initialRotation[3],
        uint16_t updateFrequency)
    : FThreadedWorker()
{
    // Allocate array for motor values
    _motorvals = new double[motorCount];

    // Create vehicle dynamics via factory method
    _dynamics = MultirotorDynamics::create();
    _newDynamics = NewMultirotorDynamics::create();

    // Initialize dynamics with initial pose
    _dynamics->init(initialPosition, initialRotation);
    _newDynamics->init(initialPosition, initialRotation);

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

FFlightManager::~FFlightManager(void)
{
    delete _dynamics;
    delete _motorvals;
}

// Called repeatedly on worker thread to compute dynamics and run flight controller (PID)
void FFlightManager::performTask(void)
{
    // Get a high-fidelity current time value from the OS
    double currentTime = FPlatformTime::Seconds() - _startTime;

    double deltaT = currentTime - _previousTime;

    if (deltaT >= _deltaT) {

        // Send current motor values to dynamics
        _dynamics->setMotors(_motorvals);

        // Update dynamics
        _dynamics->update(deltaT);

        // Get vehicle state from dynamics.  We keep pose (position, rotation) in memory for use  in
        // getKinematics() method
        
        double angularVelocityRPY[3] = {0}; // body frame
        double earthFrameAcceleration[3] = {0}; // inertial frame
        double velocityXYZ[3] = {0};        // inertial frame
        _dynamics->getState(angularVelocityRPY, earthFrameAcceleration, _rotation, velocityXYZ, _position);

        // Convert Euler angles to quaternion
        double imuOrientationQuat[4]={0};
        eulerToQuaternion(_rotation, imuOrientationQuat);

        // PID controller: update the flight manager (e.g., HackflightManager) with
        // the quaternion and gyrometer, getting the resulting motor values
        update(currentTime, imuOrientationQuat, angularVelocityRPY, _motorvals);

        // Track previous time for deltaT
        _previousTime = currentTime;
    }
}

void FFlightManager::eulerToQuaternion(double eulerAngles[3], double quaternion[4])
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

// Called by VehiclePawn::Tick() method to get current display kinematics
// (position, rotation) and propeller animation/sound (motorvals)
void FFlightManager::getKinematics(double position[3], double rotation[3], double * motorvals)
{
    for (uint8_t j=0; j<_motorCount; ++j) {
        motorvals[j] = _motorvals[j];
    }

    for (uint8_t k=0; k<3; ++k) {
        position[k] = _position[k];
        rotation[k] = _rotation[k];
    }
}
