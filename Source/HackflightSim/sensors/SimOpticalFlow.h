/*
* SimOpticalFlow.h: Class declaration for OpticalFlow class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include <SimSensor.h>

#include "GameFramework/Pawn.h"

class SimOpticalFlow : public SimSensor
{
public:

	SimOpticalFlow(APawn * pawn);
	
protected:

	virtual void modifyState(state_t & state, float time) override;

	virtual bool ready(float time) override;
};

