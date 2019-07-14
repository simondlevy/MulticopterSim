/*
   Hackflight Receiver subclass for MulticopterSim Allows us to treat an input
   device (joystick, game controller, R/C transmitter) as a "virtual receiver"
   for the firmware.

   Copyright(C) 2019 Simon D.Levy

   MIT License
   */

#pragma once

#include <receiver.hpp>

#include "Debug.hpp"

#include "../MainModule/joystick/Joystick.h"

class SimReceiver : public hf::Receiver {

    friend class FHackflightManager;

    private:

    Joystick * _joystick;

    // Helps mock up periodic availability of new data frame (output data rate; ODR)
    double _deltaT;
    double _previousTime;

    float _gimbalRoll;
    float _gimbalPitch;
    float _gimbalYaw;
    float _gimbalFOV;

    protected:

    uint8_t getAux1State(void) 
    {
        return Receiver::getAux1State();
    }

    uint8_t getAux2State(void)
    {
        // Always armed!
        return 1;
    }

    public:

    SimReceiver(uint16_t updateFrequency=50)
    {
        _joystick = new Joystick();

        _deltaT = 1./updateFrequency;
        _previousTime = 0;
    }

    void begin(void)
    {
    }

    bool gotNewFrame(void)
    {
        // Get a high-fidelity current time value from the OS
        double currentTime = FPlatformTime::Seconds();

        if (currentTime-_previousTime > _deltaT) {
            _previousTime = currentTime;
            return true;
        }

        return false;
    }

    void readRawvals(void)
    {
    }

    Joystick::error_t update(void)
    {
        // Joystick::poll() returns zero (okay) or a postive value (error)
        Joystick::error_t pollResult = _joystick->poll(rawvals);

        // In gimbal mode, grab pan,tilt from cyclic stick, then lock roll and pitch at zero
        if (!pollResult && inGimbalMode()) {

            // Get FOV from throttle, gimbal roll and yaw from cyclic stick
            _gimbalFOV   = 90 - rawvals[0] * 45 + .5; // .5 = saftey margin
            _gimbalRoll  = rawvals[1];
            _gimbalPitch = rawvals[2];
            _gimbalYaw   = rawvals[3];

            // Clamp sticks to neutral values
            rawvals[0] = 0;
            rawvals[1] = 0;
            rawvals[2] = 0;
            rawvals[3] = 0;
        }

        return pollResult;
    }

    bool inGimbalMode(void)
    {
        return rawvals[5] > AUX_THRESHOLD;
    }

    void getGimbal(float & roll, float & pitch, float & yaw, float & fov)
    {
        roll  = _gimbalRoll;
        pitch = _gimbalPitch;
        yaw   = _gimbalYaw;
        fov   = _gimbalFOV;
    }

}; // class SimReceiver
