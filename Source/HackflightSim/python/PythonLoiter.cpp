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
	// Call the Python method with the throttle and variometer values, getting the resultant throttle
	PyObject * pThrottle = PyObject_CallMethod(_pInstance, "modifyThrottle", "(ff)", demands.throttle, state.variometer);

	// Use the throttle value from Python to set the current throttle
	demands.throttle = PyFloat_AsDouble(pThrottle);
	
	// Pitch/roll
	demands.pitch = adjustCyclic(demands.pitch, state.velocityForward);
	demands.roll = adjustCyclic(demands.roll, state.velocityRightward);
}

