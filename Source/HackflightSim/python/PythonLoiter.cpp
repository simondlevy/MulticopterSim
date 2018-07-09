/*
* PythonLoiter.cpp: Class implementation for Python-based loiter in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "PythonLoiter.h"

PythonLoiter::PythonLoiter(float varioP, float varioI, float cyclicP) : 
	hf::Loiter(varioP, varioI, cyclicP),
	PythonClass("python_loiter", "PythonLoiter")
{
}

PythonLoiter::~PythonLoiter()
{
}

void PythonLoiter::modifyDemands(State & state, demands_t & demands)
{
	hf::Debug::printf("override");

	// Reset integral if moved into stick deadband
	bool inBandCurr = inBand(demands.throttle);
	if (inBandCurr && !_inBandPrev) {
		_varioIntegral = 0;
	}
	_inBandPrev = inBandCurr;


	// Throttle: inside stick deadband, adjust by variometer; outside deadband, respond weakly to stick demand
	demands.throttle = inBandCurr ? -_varioP * state.variometer - _varioI * _varioIntegral : _throttleScale * demands.throttle;

	// Pitch/roll
	demands.pitch = adjustCyclic(demands.pitch, state.velocityForward);
	demands.roll = adjustCyclic(demands.roll, state.velocityRightward);

	// Accumulate integrals
	_varioIntegral += state.variometer;
}

