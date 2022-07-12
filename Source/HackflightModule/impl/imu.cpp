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

    void gyroGetAngularVelocities(hackflight_t * hf)
    {
        hf->vstate.dphi   =  angle(Dynamics::STATE_DPHI);
        hf->vstate.dtheta = -angle(Dynamics::STATE_DTHETA); // Nose-down positive
        hf->vstate.dpsi   =  angle(Dynamics::STATE_DPSI);

       hf->gyro.isCalibrating = false;
    }

    void imuGetEulerAngles(hackflight_t * hf, uint32_t time, axes_t * angles)
    {
        (void)time;

        vehicleState_t * vstate = &hf->vstate;

        angles->x =  angle(Dynamics::STATE_PHI);
        angles->y = -angle(Dynamics::STATE_THETA); // Nose-down positive
        angles->z =  angle(Dynamics::STATE_PSI);
    }

} // extern "C"
