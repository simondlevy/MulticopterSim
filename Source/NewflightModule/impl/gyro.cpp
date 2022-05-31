#include <stdint.h>

#define _USE_MATH_DEFINES
#include <math.h>

#include <gyro.h>
#include <rad2deg.h>

#include "../../MainModule/Dynamics.hpp"

static Dynamics * _dynamics;

void shareDynamics(Dynamics * dynamics)
{
    _dynamics = dynamics;
}

extern "C" {

void gyroReadScaled(vehicle_state_t * state, bool * isCalibrating)
{
    state->dphi   = rad2deg(_dynamics->x(Dynamics::STATE_PHI_DOT));
    state->dtheta = rad2deg(_dynamics->x(Dynamics::STATE_THETA_DOT));
    state->dpsi   = rad2deg(_dynamics->x(Dynamics::STATE_PSI_DOT));

    *isCalibrating = false;
}

} // extern "C"
