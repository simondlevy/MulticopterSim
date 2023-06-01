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

#include "../Thread.hpp"

class FLocalThread : public FVehicleThread {

    protected:

        virtual void getMotors(
                const double time,
                const float * joyvals,
                const Dynamics * dynamics,
                float * motorValues,
                const uint8_t motorCount) override
        {
            for (auto k=0; k<motorCount; ++k) {
                motorValues[k] = 0.6;
            }
        }

    public:

        // Constructor, called main thread
        FLocalThread(Dynamics * dynamics)
            : FVehicleThread(dynamics)
        {
        }

        ~FLocalThread(void) 
        {
        }

 }; // class FLocalThread
