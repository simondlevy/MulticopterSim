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
}

SimBoard::~SimBoard()
{
}	

TArray<float> SimBoard::update(float currentTime, FQuat quat, FVector gyro)
{
    _currentTime = currentTime;

	_quat = quat;
	_gyro = gyro;

    return _motors;
}

bool SimBoard::getQuaternion(float quat[4])
{
	quat[0] = _quat.W;
	quat[1] = -_quat.X; // invert X
	quat[2] = -_quat.Y;	// invert Y
	quat[3] = _quat.Z;

	return true;
}

bool SimBoard::getGyrometer(float gyro[3])
{
	gyro[0] = _gyro.X;
	gyro[1] = _gyro.Y;
	gyro[2] = 0; // XXX ignore Z for now

    return true;
}

void SimBoard::writeMotor(uint8_t index, float value)
{
	_motors[index] = value;
}

float SimBoard::getTime(void)
{
    return _currentTime;
}
