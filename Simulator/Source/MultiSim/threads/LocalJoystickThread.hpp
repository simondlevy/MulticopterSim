/* Vehicle thread for joystick values coming from compiled simulator
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#define WIN32_LEAN_AND_MEAN

#include "../Joystick.h"
#include "../Thread.hpp"

class FLocalJoystickThread : public FVehicleThread {

    private:

        // Joystick / game controller / RC transmitter
        IJoystick * _joystick;

    protected:

        virtual void getActuators(
                const Dynamics * dynamics,
                const double timeSec,
                const uint8_t actuatorCount,
                float * actuatorValues) override
        {
            float joyvals[10] = {};
            _joystick->poll(joyvals);

            getActuators(
                    dynamics, timeSec, joyvals, actuatorCount, actuatorValues);
        }

        virtual void getActuators(
                const Dynamics * dynamics,
                const double timeSec,
                const float * joyvals,
                const uint8_t actuatorCount,
                float * actuatorValues) = 0;

    public:

        // Constructor, called main thread
        FLocalJoystickThread(
                Dynamics * dynamics,
                const uint32_t controlPeriod = 100)
            : FVehicleThread(dynamics, controlPeriod)
        {
            _joystick = new IJoystick();
        }

        ~FLocalJoystickThread(void)
        {
        }

}; // class FLocalJoystickThread
