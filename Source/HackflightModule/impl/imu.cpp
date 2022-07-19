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

    void gyroReadScaled(hackflight_t * hf, vehicleState_t * vstate)
    {
        vstate->dphi   =  rad2deg(_dynamics->vstate.dphi); 
        vstate->dtheta = -rad2deg(_dynamics->vstate.dtheta); // Nose-down positive
        vstate->dpsi   =  rad2deg(_dynamics->vstate.dpsi);

       hf->gyro.isCalibrating = false;
    }

    void imuGetEulerAngles(hackflight_t * hf, uint32_t time, axes_t * angles)
    {
        (void)time;

        vehicleState_t * vstate = &hf->vstate;

        angles->x =  rad2deg(_dynamics->vstate.phi);
        angles->y = -rad2deg(_dynamics->vstate.theta); // Nose-down positive
        angles->z =  rad2deg(_dynamics->vstate.psi);
    }

} // extern "C"
