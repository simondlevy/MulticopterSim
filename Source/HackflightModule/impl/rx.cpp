#include <stdint.h>

#include <datatypes.h>
#include <debug.h>
#include <rx.h>
#include <time.h>

#include "../../MainModule/Joystick.h"
#include "../../MainModule/Utils.hpp"

static const int16_t AXIS_SCALE = 669;

static const float THROTTLE_LOW_THRESHOLD = 0.02;

static IJoystick * _joystick;
static double      _joyvals[10];


extern "C" {

void rxGetDemands(
        rx_t * rx,
        uint32_t currentTimeUs,
        angle_pid_t * anglepid,
        demands_t * demands)
{
    (void)rx;
    (void)currentTimeUs;
    (void)anglepid;
    
    demands->throttle = _joyvals[0];

    // [-1,+1] => [-AXIS_SCALE,+AXIS_SCALE]
    demands->roll  = AXIS_SCALE * _joyvals[1]; 
    demands->pitch = AXIS_SCALE * _joyvals[2];
    demands->yaw   = AXIS_SCALE * _joyvals[3];
}

void rxPoll(
        rx_t * rx,
        uint32_t currentTimeUs,
        bool imuIsLevel,
        bool calibrating,
        rx_axes_t * rxax,
        void * motorDevice,
        arming_t * arming,
        bool * pidItermResetReady,
        bool * pidItermResetValue,
        bool * gotNewData)
{
    (void)rx;
    (void)currentTimeUs;
    (void)imuIsLevel;
    (void)motorDevice;
    (void)arming;

    if (!_joystick) {
        _joystick = new IJoystick();
    }

    _joystick->poll(_joyvals);

    _joyvals[0] = (_joyvals[0] + 1) / 2; // [-1,+1] => [0,1]

    rxax->demands.throttle = _joyvals[0];

    bool lowThrottle = _joyvals[0] < THROTTLE_LOW_THRESHOLD;

    *pidItermResetReady = true;
    *pidItermResetValue = lowThrottle;

    *gotNewData = true;
}

} // extern "C"

