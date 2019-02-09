/*
 * BuiltinPhysics.cpp: Physics class implemenation using UE4 built-in physics
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "BuiltinPhysics.h"

FVector BuiltinPhysics::getAccelerometer(float velocityZ, FVector & euler, float deltaSeconds)
{
    // Use velocity first difference to emulate G force on vehicle in inertial frame
    float vario = velocityZ / 100; // m/s
    float gs = ((vario - _varioPrev) / deltaSeconds + AVehiclePawn::G) / AVehiclePawn::G;
    _varioPrev = vario;

    // Convert inertial frame to body frame
    // See slide 50 from https://slideplayer.com/slide/2813564/
    float phi = euler.X;
    float theta = euler.Y;
    return gs * FVector(-sin(theta), sin(phi)*cos(theta), cos(phi)*cos(theta));
}

FQuat BuiltinPhysics::getQuaternion(class AVehiclePawn * vehiclePawn)
{
    // Get current quaternion and convert it to our format (XXX necessary?
    FQuat quat = vehiclePawn->GetActorQuat();
    quat.X = -quat.X;
    quat.Y = -quat.Y;
    return quat;
}

BuiltinPhysics::BuiltinPhysics(void)
{
    // Start the "receiver" (joystick/gamepad)
    receiver = new hf::SimReceiver();

    // Start Hackflight firmware, indicating already armed
    hackflight.init(this, receiver, &mixer, &ratePid, true);

    // Add optical-flow sensor
    //hackflight.addSensor(&_flowSensor);

    // Add rangefinder
    //hackflight.addSensor(&_rangefinder);

    // Add level PID controller for aux switch position 1
    hackflight.addPidController(&level, 1);

    // Add loiter PID controllers for aux switch position 2
    hackflight.addPidController(&althold, 2);
    //hackflight.addPidController(&poshold, 2);
}

void BuiltinPhysics::start(void)
{
    // Initialize simulation variables
    _varioPrev = 0;
    _elapsedTime = 0;
    _eulerXPrev = 0;
    _eulerYPrev = 0;
}

TArray<float> BuiltinPhysics::update(float deltaSeconds, AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh)
{
    // Update the receiver
    receiver->update();

    // Update the Hackflight firmware
    hackflight.update();

    // Convert quaternion to Euler angles
    FVector euler = FMath::DegreesToRadians(vehiclePawn->GetActorQuat().Euler());

    // Get the simulated IMU readings
    FQuat   quat = getQuaternion(vehiclePawn);

    // Store quaternion and gyro values for Hackflight::Board methods below
    _quat[0] = quat.W;
    _quat[1] = quat.X;
    _quat[2] = quat.Y;
    _quat[3] = quat.Z;
    _gyro[0] = (euler.X - _eulerXPrev) / deltaSeconds;
    _gyro[1] = (euler.Y - _eulerYPrev) / deltaSeconds;
    _gyro[2] = 0; // zero-out gyro Z for now

    // Store current Euler X,Y for gyro simulation
    _eulerXPrev = euler.X;
    _eulerYPrev = euler.Y;

    TArray<float> motorvals = { _motorvals[0], _motorvals[1], _motorvals[2], _motorvals[3] };

    // Use physics model to compute rotation and translation forces on vehicle
    FVector rotationForce = { 0,0,0 };
    FVector translationForce = { 0,0,0 };
    computeForces(deltaSeconds, motorvals, euler, rotationForce, translationForce);

    // Add movement force vector to vehicle 
    vehicleMesh->AddForce(translationForce);

    // Add rotation to vehicle 
    vehiclePawn->AddActorLocalRotation(deltaSeconds * FRotator(rotationForce.Y, rotationForce.Z, rotationForce.X) * (180 / M_PI));

    // Output the motor values for audiovisual effect
    return motorvals;
}

void BuiltinPhysics::computeForces(float deltaSeconds, TArray<float> motorValues, FVector & euler, FVector & rotationForce, FVector & translationForce)
{
    // Convert motor values to rotational forces
    rotationForce.X = motorsToAngularForce(motorValues, 2, 3, 0, 1);
    rotationForce.Y = motorsToAngularForce(motorValues, 1, 3, 0, 2);
    rotationForce.Z = motorsToAngularForce(motorValues, 1, 2, 0, 3);

    // Rotate Euler angles into inertial frame: http://www.chrobotics.com/library/understanding-euler-angles
    float x = sin(euler.X)*sin(euler.Z) + cos(euler.X)*cos(euler.Z)*sin(euler.Y);
    float y = cos(euler.X)*sin(euler.Y)*sin(euler.Z) - cos(euler.Z)*sin(euler.X);
    float z = cos(euler.Y)*cos(euler.X);

    // Use rotated Euler angles to compute translation force
    translationForce = THRUST_FACTOR * sum(motorValues) * FVector(-x, -y, z);
}

float BuiltinPhysics::motorsToAngularForce(TArray<float> motorValues, uint8_t a, uint8_t b, uint8_t c, uint8_t d)
{
    float v = ((motorValues[a] + motorValues[b]) - (motorValues[c] + motorValues[d]));

    return (v < 0 ? -1 : +1) * fabs(v);
}

float BuiltinPhysics::sum(TArray<float> x)
{
    float s = 0.f;

    for (auto it = x.CreateConstIterator(); it; ++it) {
        s += *it;
    }

    return s;
}

// Hackflight::Board method implementation -------------------------------------

bool BuiltinPhysics::getQuaternion(float quat[4])
{
    memcpy(quat, _quat, 4*sizeof(float));
    return true;
}

bool BuiltinPhysics::getGyrometer(float gyro[3])
{
    memcpy(gyro, _gyro, 3*sizeof(float));
    return true;
}

void BuiltinPhysics::writeMotor(uint8_t index, float value)
{
    _motorvals[index] = value;
}

float BuiltinPhysics::getTime(void)
{
    // Track elapsed time
    _elapsedTime += .01; // Assume 100Hz clock

    return _elapsedTime;
}

uint8_t	BuiltinPhysics::serialAvailableBytes(void)
{
    return 0; // XXX
}

uint8_t	BuiltinPhysics::serialReadByte(void)
{
    return 0; // XXX
}

void BuiltinPhysics::serialWriteByte(uint8_t c)
{ // XXX
}
