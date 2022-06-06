#include <stdint.h>

#define _USE_MATH_DEFINES
#include <math.h>

#include <time.h>
#include <datatypes.h>
#include <gyro.h>
#include <rad2deg.h>

#include "../../MainModule/Dynamics.hpp"

static Dynamics * _dynamics;

void shareDynamics(Dynamics * dynamics)
{
    _dynamics = dynamics;
}

extern "C" {

    static float angle(uint8_t index)
    {
        return rad2deg(_dynamics->x(index));
    }

    void gyroReadScaled(vehicle_state_t * vstate, bool * isCalibrating)
    {
        vstate->dphi   =  angle(Dynamics::STATE_DPHI);
        vstate->dtheta = -angle(Dynamics::STATE_DTHETA); // Nose-down positive
        vstate->dpsi   =  angle(Dynamics::STATE_DPSI);

        *isCalibrating = false;
    }

    void imuAccumulateGyro(float * adcf)
    {
        (void)adcf;
    }

    void imuGetEulerAngles(timeUs_t time, vehicle_state_t * vstate, bool armed)
    {
        (void)time;
        (void)armed;

        vstate->phi   =  angle(Dynamics::STATE_PHI);
        vstate->theta = -angle(Dynamics::STATE_THETA); // Nose-down positive
        vstate->psi   =  angle(Dynamics::STATE_PSI);

    }

} // extern "C"
