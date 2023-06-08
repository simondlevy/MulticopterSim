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

class FNewRemoteThread : public FVehicleThread {

    protected:

        virtual void getMotors(
                const double time,
                const float * joyvals,
                const Dynamics * dynamics,
                float * motorValues,
                const uint8_t motorCount) override
        {
            static long _count;
            sprintf_s(_message, "Waiting on client: %ld", _count++);
        }

    public:

        // Constructor, called main thread
        FNewRemoteThread(Dynamics * dynamics)
            : FVehicleThread(dynamics)
        {
        }

        ~FNewRemoteThread(void) 
        {
        }

 }; // class FNewRemoteThread
