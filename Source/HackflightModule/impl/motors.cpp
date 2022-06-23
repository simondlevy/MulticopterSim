#include <stdint.h>

#include <motor.h>

static float * _motors;

void shareMotors(float * motors)
{
    _motors = motors;
}

extern "C" {

bool motorIsProtocolDshot(void)
{
    return false;
}

float motorValueDisarmed(void)
{
    return 0;
}

float motorValueHigh(void)
{
    return 1;
}

float motorValueLow(void)
{
    return 0;
}

void motorWrite(void * motorDevice, float * values)
{
    (void)motorDevice;

    memcpy(_motors, values, 4*sizeof(float));
}

} // extern "C"
