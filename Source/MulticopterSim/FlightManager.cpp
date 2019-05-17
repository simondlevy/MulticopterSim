/*
 * ThreadedWorker subclass for dynamics computation
 *
 * This class contains very little code, because the dynamics computations are 
 * done in the platform-independent MultirotorDynamics class.
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "FlightManager.h"
#include "VehiclePawn.h"

// Called once on main thread
FFlightManager::FFlightManager(class AVehiclePawn * vehiclePawn, uint8_t motorCount,
        double initialPosition[3], double initialRotation[3]) : FThreadedWorker()
{
    // Store vehicle pawn for use by subclasses
    _vehiclePawn = vehiclePawn;
    
    // Allocate array for motor values
    _motorvals = new double[motorCount];

    // Create vehicle dynamics via factory method
    _dynamics = MultirotorDynamics::create();

    // Initialize dynamics with initial pose
    _dynamics->init(initialPosition, initialRotation);

    _motorCount = motorCount;
    _previousTime = 0;
}

FFlightManager::~FFlightManager(void)
{
    delete _dynamics;
    delete _motorvals;
}

// Called repeatedly on worker thread
void FFlightManager::performTask(void)
{
    double currentTime = FPlatformTime::Seconds();


    sprintf_s(_message, "%f", currentTime-_previousTime);

    if (_previousTime>0) {

        //_dynamics->update(currentTime-_previousTime);
    }

    _previousTime = currentTime;
}

void FFlightManager::getPoseAndMotors(double deltaT, double position[3], double rotation[3], double * motorvals)
{
    // Send current motor values to dynamics
    _dynamics->setMotors(_motorvals);

    // Update dynamics
    _dynamics->update(deltaT);

    // Get vehicle state from dynamics
    double angularVelocityRPY[3] = {0}; // body frame
    double velocityXYZ[3] = {0};        // inertial frame
    _dynamics->getState(angularVelocityRPY, rotation, velocityXYZ, position);

    // Convert Euler angles to quaternion
    double imuOrientationQuat[4]={0};
    MultirotorDynamics::eulerToQuaternion(rotation, imuOrientationQuat);

    // PID controller: update the flight manager with the quaternion and gyrometer, getting the resulting motor values
    update(deltaT, imuOrientationQuat, angularVelocityRPY, _motorvals);

    // Copy out motor values
    for (uint8_t j=0; j<_motorCount; ++j) {
        motorvals[j] = _motorvals[j];
    }
}
