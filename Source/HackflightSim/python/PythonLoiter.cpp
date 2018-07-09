#include "PythonLoiter.h"



PythonLoiter::PythonLoiter(float varioP, float varioI, float cyclicP) : hf::Loiter(varioP, varioI, cyclicP)
{
}


PythonLoiter::~PythonLoiter()
{
}

void PythonLoiter::modifyDemands(State & state, demands_t & demands)
{
	hf::Debug::printf("override");

	//Debug::printf("var: %+6.6f  varint: %+6.6f  for: %+4.4f   rgt: %+4.4f\n",
	//        state.variometer, _varioIntegral, state.velocityForward, state.velocityRightward);

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

