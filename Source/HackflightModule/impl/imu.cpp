#include <stdint.h>

#include <time.h>
#include <datatypes.h>

extern "C" {

void imuAccumulateGyro(float * adcf)
{
    (void)adcf;
}

void imuGetQuaternion(uint32_t time, bool armed, quaternion_t * quat)
{
    // XXX stubbed for now
    quat->w = 1;
    quat->x = 0;
    quat->y = 0;
    quat->z = 0;
}

void imuUpdateFusion(timeUs_t time, quaternion_t * quat, rotation_t * rot)
{
    (void)time;
    (void)quat;
    (void)rot;
}

} // extern "C"
