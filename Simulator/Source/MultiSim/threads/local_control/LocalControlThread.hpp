/*
 * Local flight-management stub for MultiSim
 *
 * Spins all motors at 60%
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../LocalJoystickThread.hpp"

// Un-comment for Hackflight support
#define HACKFLIGHT

#ifdef HACKFLIGHT
#include "hackflight.hpp"
#endif

class FLocalControlThread : public FLocalJoystickThread {

#ifdef HACKFLIGHT
    private:

        HackflightForSim _hf;
#endif

    protected:

        virtual void getActuators(

                const Dynamics * dynamics,
                const double timeSec,
                const float * joyvals,
                const uint8_t actuatorCount,
                float * actuatorValues) override
        {
#ifdef HACKFLIGHT
            _hf.step(
                    timeSec, joyvals, dynamics, actuatorCount, actuatorValues);
#else
            for (auto k=0; k<actuatorCount; ++k) {
                actuatorValues[k] = 0.6;
            }
#endif
        }

    public:

        // Constructor, called main thread
        FLocalControlThread(Dynamics * dynamics)
            : FLocalJoystickThread(dynamics)
        {
        }

        ~FLocalControlThread(void) 
        {
        }

 }; // class FLocalControlThread
