/*
* HackflightSimOpticalFlow.h: Class declaration for OpticalFlow class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include <sensors/opticalflow.hpp>

#include "GameFramework/Pawn.h"

class HackflightSimOpticalFlow : public hf::OpticalFlow
{
public:

	HackflightSimOpticalFlow(APawn * pawn);
	
protected:

	virtual void getFlow(float flow[2]) override;

private:

	APawn * _pawn;
};

