/*
* SimBoard.cpp: Hackflight::Board class implementation in MulticopterSim
*
* Copyright(C) 2018 Simon D.Levy
*
* MIT License
*/

#include "SimBoard.h"

#include <debug.hpp>

SimBoard::SimBoard()
{
    _elapsedTime = 0;

    _quat[0] = 0;
    _quat[1] = 0;
    _quat[2] = 0;
    _quat[3] = 0;

    _gyro[0] = 0;
    _gyro[1] = 0;
    _gyro[2] = 0;

    _motors[0] = 0;
    _motors[1] = 0;
    _motors[2] = 0;
    _motors[3] = 0;
}


SimBoard::~SimBoard()
{
}	

TArray<float> SimBoard::update(float deltaTime, FQuat quat, FVector gyro)
{
    _elapsedTime += deltaTime;

    _quat[0] = quat.W;
    _quat[1] = -quat.X;
    _quat[2] = -quat.Y;
    _quat[3] = quat.Z;

	_gyro[0] = gyro.X;
    _gyro[1] = gyro.Y;
    _gyro[2] = 0; // zero-out gyro Z for now

    TArray<float> motorvals = {_motors[0], _motors[1], _motors[2], _motors[3]};
    return motorvals;
}

bool SimBoard::getQuaternion(float quat[4])
{
    memcpy(quat, _quat, 4*sizeof(float));
    return true;
}

bool SimBoard::getGyrometer(float gyro[3])
{
    memcpy(gyro, _gyro, 3*sizeof(float));
    return true;
}

void SimBoard::writeMotor(uint8_t index, float value)
{
	_motors[index] = value;
}

float SimBoard::getTime(void)
{
    return _elapsedTime;
}
