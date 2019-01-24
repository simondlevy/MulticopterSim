/*
* SimSensor.h: Superclass declaration for sensor classes in MulticopterSim
*
* Copyright(C) 2018 Simon D.Levy
*
* MIT License
*/ 

#pragma once

#include "GameFramework/Pawn.h"
#include <sensor.hpp>

class SimSensor : public hf::Sensor
{

public:

	SimSensor(APawn * pawn);

protected:

	FVector getEulerAngles(void);

	APawn * _pawn;
};

