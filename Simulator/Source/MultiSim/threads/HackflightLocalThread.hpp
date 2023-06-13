/*
 * Hacfklight local flight-management class for MultiSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Thread.hpp"
#include "hackflight.hpp"

class FLocalThread : public FVehicleThread {
    
    private:

        HackflightForSim _hf;

    protected:

        virtual void getMotors(
                const double time,
                const float * joyvals,
                const Dynamics * dynamics,
                float * motorValues,
                const uint8_t motorCount) override
        {
            _hf.getMotors(time, joyvals, dynamics, motorValues, motorCount);
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
