/*
* SimSensor.cpp: Superclass methods for sensor classes in MulticopterSim
*
* Copyright(C) 2018 Simon D.Levy
*
* MIT License
*/

#include "SimSensor.h"

SimSensor::SimSensor(APawn * pawn)
{
	_pawn = pawn;
}

FVector SimSensor::getEulerAngles(void) 
{
	return FMath::DegreesToRadians(_pawn->GetActorQuat().Euler());
}
