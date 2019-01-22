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
#include <HackflightSimSensor.h>
#include "GameFramework/Pawn.h"

class HackflightSimRangefinder : public HackflightSimSensor
{
public:

	HackflightSimRangefinder(APawn * pawn);

protected:

	virtual void modifyState(state_t & state, float time) override;

	virtual bool ready(float time) override;

private:

    bool _ready = false;

	float _groundAltitude;

	float getAltitude(void);
};

