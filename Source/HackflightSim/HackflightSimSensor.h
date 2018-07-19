/*
* HackflightSimSensor.h: Superclass declaration for sensor classes in HackflightSim
*
* Copyright(C) 2018 Simon D.Levy
*
* MIT License
*/ 

#pragma once

#include "GameFramework/Pawn.h"

class HackflightSimSensor
{

public:

	HackflightSimSensor(APawn * pawn);

protected:

	FVector getEulerAngles(void);

	APawn * _pawn;
};

