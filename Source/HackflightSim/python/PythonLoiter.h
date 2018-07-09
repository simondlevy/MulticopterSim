#pragma once

#define _PYTHON

#ifdef _PYTHON

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

#include <loiter.hpp>
using namespace hf;

class PythonLoiter : public hf::Loiter {

public:

	PythonLoiter(float varioP, float varioI, float cyclicP);

	~PythonLoiter();

protected:

	virtual void modifyDemands(State & state, demands_t & demands) override;
};

#endif
