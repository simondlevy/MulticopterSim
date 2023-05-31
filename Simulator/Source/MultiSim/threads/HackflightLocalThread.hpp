/*
 * Hacfklight local flight-management class for MultiSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Thread.hpp"

// Supported by  ../MultiSim.Build.cs
#include <core/pid.h>
#include <core/pids/angle.h>
#include <core/pids/setpoints/althold.h>
#include <core/pids/setpoints/flowhold.h>
#include <core/mixers/fixedpitch/quadxbf.h>

class FLocalThread : public FVehicleThread {
    
    private:

        AnglePidController anglePid = 
            AnglePidController(
                    10, // K_rate_p
                    10, // K_rate_i
                    1,  // K_rate_d
                    0,  // K_rate_f
                    4); // K_level_p

        AltHoldPidController altHoldPid;

        FlowHoldPidController flowHoldPid;

        Mixer mixer = QuadXbfMixer::make();

        std::vector<PidController *> pids = {
            &anglePid, &altHoldPid, &flowHoldPid
        };

    protected:

        virtual void getMotors(
                const double time,
                const double * joyvals,
                const Dynamics * dynamics,
                double * motors,
                const uint8_t motorCount) override
        {
            // Convert simulator time to microseconds
            const auto usec = (uint32_t)(time * 1e6);

            /*
            // Build vehicle state 
            auto vstate = state_from_telemetry(telemetry);

            // Use heading angle to rotate dx, dy into vehicle coordinates
            rotateToVehicleFrame(vstate);

            // Build stick demands
            auto demands = demands_from_telemetry(telemetry);

            // Reset PID controllers on zero throttle
            auto pidReset = demands.throttle < .05;

            // Run stick demands through PID controllers to get final demands
            PidController::run(pids, demands, vstate, usec, pidReset);

            // Run final demands through mixer to get motor values
            float mvals[4] = {};
            mixer.getMotors(demands, mvals);
            */

            for (auto k=0; k<motorCount; ++k) {
                motors[k] = 0.6; // mvals[k];
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
