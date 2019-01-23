/*
* PythonLoiter.h: Class declaration for Python-based loiter in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "python_class.h"

#ifdef _PYTHON

#include <Python.h>

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include <pidcontrollers/loiter.hpp>
using namespace hf;

class PythonLoiter : public hf::Loiter, public PythonClass {

public:

	PythonLoiter(float altitudeP, float altitudeD, float cyclicP);

	~PythonLoiter();

	void start(void);

protected:

	virtual bool modifyDemands(State & state, demands_t & demands) override;

};

#endif
