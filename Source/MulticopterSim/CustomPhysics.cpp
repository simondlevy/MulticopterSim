/*
 * CustomPhysics.h: Physics class using custom physics/dynamics model
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "CustomPhysics.h"
#include "debug.h"

CustomPhysics::CustomPhysics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh) : Physics(vehiclePawn, vehicleMesh)

{
    _flightManager = FlightManager::createFlightManager(vehiclePawn);

    // Turn off UE4 physics
	_vehicleMesh->SetSimulatePhysics(false);

}

void CustomPhysics::start(void)
{
    // Get vehicle ground-truth location and rotation to initialize dynamics
    FVector pos = _vehiclePawn->GetActorLocation() / 100; // cm => m
    double groundTruthPosition[3] = {pos.X, pos.Y, pos.Z};
    FRotator rot = _vehiclePawn->GetActorRotation(); 
    double groundTruthRotation[3] = {rot.Roll, rot.Pitch, rot.Yaw};

    _dynamics.init(groundTruthPosition, groundTruthRotation);
}

TArray<float> CustomPhysics::update(float deltaSeconds)
{
    // Current gyro values
    static double gyro[3];

    // Current motor values
    static double _motorvals[4];

    // Get vehicle state by passing motor values to dynamics
    double imuAngularVelocityRPY[3] = {0}; // body frame
    double eulerAngles[3] = {0};           // body frame
    double velocityXYZ[3] = {0};           // inertial frame
    double positionXYZ[3] = {0};           // inertial frame
    _dynamics.update(
                deltaSeconds, 
                _motorvals, 
                imuAngularVelocityRPY, 
                eulerAngles, 
                velocityXYZ, 
                positionXYZ);

    // Set pawn location using position from dynamics
    _vehiclePawn->SetActorLocation(FVector(positionXYZ[0], positionXYZ[1], positionXYZ[2]) * 100); // m =>cm

    // Set pawn rotation using Euler angles (note order: pitch, yaw, roll = 1,2,0 = Y,Z,X)
    _vehiclePawn->SetActorRotation(FRotator(eulerAngles[1], eulerAngles[2], eulerAngles[0]) * (180 / M_PI)); // radians => deg

    // Convert Euler angles to quaternion
    double imuOrientationQuat[4]={0};
    MultirotorDynamics::eulerToQuaternion(eulerAngles, imuOrientationQuat);

    // PID controller: update the flight manager with the quaternion and gyrometer, getting the resulting motor values
    // Note quaternion order: https://api.unrealengine.com/INT/API/Runtime/Core/Math/FQuat/__ctor/7/index.html    
    TArray<float> motorvals = _flightManager->update(
            deltaSeconds, 
            FQuat(imuOrientationQuat[1], imuOrientationQuat[2], imuOrientationQuat[3], imuOrientationQuat[0]), 
            FVector(imuAngularVelocityRPY[0], imuAngularVelocityRPY[1], imuAngularVelocityRPY[2]));

    // Set motor values for dynamics on next iteration
    for (uint8_t j=0; j<4; ++j) {
        _motorvals[j] = motorvals[j];
    }

    // Return the motor values for audiovisual effect
    return motorvals;
}

// Factory method for Physics class
Physics * Physics::createPhysics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh)
{
    return new CustomPhysics(vehiclePawn, vehicleMesh);
}
