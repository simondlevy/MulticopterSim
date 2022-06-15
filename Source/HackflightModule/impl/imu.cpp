#include <stdint.h>

#define _USE_MATH_DEFINES
#include <math.h>

#include <datatypes.h>
#include <gyro.h>
#include <imu.h>
#include <rad2deg.h>
#include <time.h>

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

    void gyroReadScaled(gyro_t * gyro, vehicle_state_t * vstate)
    {
        vstate->dphi   =  angle(Dynamics::STATE_DPHI);
        vstate->dtheta = -angle(Dynamics::STATE_DTHETA); // Nose-down positive
        vstate->dpsi   =  angle(Dynamics::STATE_DPSI);

       gyro->isCalibrating = false;
    }


    void imuGetEulerAngles(hackflight_t * hf, uint32_t time)
    {
        (void)time;

        vehicle_state_t * vstate = &hf->vstate;

        vstate->phi   =  angle(Dynamics::STATE_PHI);
        vstate->theta = -angle(Dynamics::STATE_THETA); // Nose-down positive
        vstate->psi   =  angle(Dynamics::STATE_PSI);
    }

} // extern "C"
