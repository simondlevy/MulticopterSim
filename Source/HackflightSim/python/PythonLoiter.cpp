/*
* PythonLoiter.cpp: Class implementation for Python-based loiter in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "PythonLoiter.h"

#ifdef _PYTHON

PythonLoiter::PythonLoiter(float varioP, float varioI, float cyclicP) : 
	hf::Loiter(varioP, varioI, cyclicP),
	PythonClass("nengo_picontrol", "PIController")
{
	// Setup args for constructor
	PyObject * pArgs = PyTuple_New(2);
	PyTuple_SetItem(pArgs, 0, PyFloat_FromDouble(varioP));
	PyTuple_SetItem(pArgs, 1, PyFloat_FromDouble(varioI));

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
		PyObject_CallMethod(_pInstance, "reset", "()");
	}
	_inBandPrev = inBandCurr;

	// Use the throttle value from Python to set the current throttle
	demands.throttle = inBandCurr ? 
		PyFloat_AsDouble(PyObject_CallMethod(_pInstance, "getCorrection", "(f)", state.variometer)) : 
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
