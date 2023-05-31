/*
 * Local flight-management class for MultiSim
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
                const double * joyvals,
                const Dynamics * dynamics,
                double * motors,
                const uint8_t motorCount) override
        {
            for (auto k=0; k<motorCount; ++k) {
                motors[k] = 0.6;
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
