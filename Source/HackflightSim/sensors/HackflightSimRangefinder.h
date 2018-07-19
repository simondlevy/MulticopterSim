/*
* HackflightSimRangefinder.h: Class declaration for Rangefinder class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include <sensors/rangefinder.hpp>

#include "GameFramework/Pawn.h"

class HackflightSimRangefinder : public hf::Rangefinder
{
public:

	HackflightSimRangefinder(APawn * pawn);

	void init(void);
	
protected:

	virtual bool distanceAvailable(float &distance) override;

private:

	APawn * _pawn;

	float _groundAltitude;

	float getAltitude(void);
};

