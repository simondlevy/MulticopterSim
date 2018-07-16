/*
* PythonLoiter.cpp: Class implementation for Python-based loiter in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "PythonLoiter.h"

#ifdef _PYTHON

PythonLoiter::PythonLoiter(float altitudeP, float altitudeD, float cyclicP) : 
	hf::Loiter(altitudeP, altitudeD, cyclicP),
	PythonClass("nengo_pidcontrol", "PIDController")
{
	// Setup args for constructor
	PyObject * pArgs = PyTuple_New(2);
	PyTuple_SetItem(pArgs, 0, PyFloat_FromDouble(altitudeP));
	PyTuple_SetItem(pArgs, 1, PyFloat_FromDouble(altitudeD));

	// Create class instance with args
	_pInstance = PyObject_CallObject(_pClass, pArgs);
}

PythonLoiter::~PythonLoiter()
{
}

void PythonLoiter::modifyDemands(State & state, demands_t & demands)
{
	// Reset integral if moved into stick deadband
	bool inBandCurr = inBand(demands.throttle);
	if (inBandCurr && !_inBandPrev) {
		_altitudeTarget = state.altitude;
	}
	_inBandPrev = inBandCurr;

    // Inside throttle deadband, adjust pitch/roll demand by PD controller; outside deadband, leave it as-is
	demands.throttle = inBand(demands.throttle) ?
		PyFloat_AsDouble(PyObject_CallMethod(_pInstance, "getCorrection", "(ff)", _altitudeTarget, state.altitude)) :
		_throttleScale*demands.throttle;
	
	// Adjust pitch/roll using the C++ super-class
	demands.pitch = adjustCyclic(demands.pitch, state.velocityForward);
	demands.roll = adjustCyclic(demands.roll, state.velocityRightward);
}

void PythonLoiter::start(void)
{
	PyObject_CallMethod(_pInstance, "start", "()");
}

#endif
