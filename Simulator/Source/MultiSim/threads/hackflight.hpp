/*
 * Hacfklight support for MulticopterSim
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "dynamics/fixedpitch/QuadXBF.hpp"

// Supported by  ../MultiSim.Build.cs
#include <core/pid.h>
#include <core/pids/angle.h>
#include <core/pids/setpoints/althold.h>
#include <core/mixers/fixedpitch/quadxbf.h>

class HackflightForSim {
    
    private:

        AnglePidController anglePid = 
            AnglePidController(
                    10, // K_rate_p
                    10, // K_rate_i
                    1,  // K_rate_d
                    0,  // K_rate_f
                    4); // K_level_p

        AltHoldPidController altHoldPid;

        Mixer mixer = QuadXbfMixer::make();

        std::vector<PidController *> pids = { &anglePid, &altHoldPid };

        static Demands demands_from_joystick(const float joystick[])
        {
            return Demands(
                    (joystick[0] + 1) / 2, // [-1,+1] => [0,1]
                    joystick[1],
                    joystick[2],
                    joystick[3]
                    );
        }

        static float rad2deg(const double rad)
        {
            return (float)(180 * rad / M_PI);
        }

        static VehicleState state_from_dynamics(const Dynamics * dynamics)
        {
            Dynamics::vehicle_state_t vstate = dynamics->vstate;

            return VehicleState( 
                    (float)vstate.x,
                    (float)vstate.dx,
                    (float)vstate.y,
                    (float)vstate.dy,
                    -(float)vstate.z,  // NED => ENI
                    -(float)vstate.dz, // NED => ENI
                    rad2deg(vstate.phi),
                    rad2deg(vstate.dphi),
                    -rad2deg(vstate.theta),  // note sign reveral
                    -rad2deg(vstate.dtheta), // note sign reveral
                    rad2deg(vstate.psi),
                    rad2deg(vstate.dpsi)
                    );
        }

        static void rotateToVehicleFrame(VehicleState & vstate)
        {
            const auto psi = vstate.psi * M_PI / 180;
            const auto dx = cos(psi) * vstate.dx + sin(psi) *  vstate.dy;
            const auto dy = sin(psi) * vstate.dx + cos(psi) *  vstate.dy;
            vstate.dx = dx;
            vstate.dy = dy;
        }

    public:

        virtual void step(
                const double time,
                const float * sticks,
                const Dynamics * dynamics,
                float * motorValues,
                const uint8_t motorCount)
        {
            // Convert simulator time to microseconds
            const auto usec = (uint32_t)(time * 1e6);

            // Build stick demands
            auto demands = demands_from_joystick(sticks);

            // Build vehicle state 
            auto vstate = state_from_dynamics(dynamics);

            // Use heading angle to rotate dx, dy into vehicle coordinates
            rotateToVehicleFrame(vstate);

            // Reset PID controllers on zero throttle
            auto pidReset = demands.throttle < .05;

            // Run stick demands through PID controllers to get final demands
            PidController::run(pids, demands, vstate, usec, pidReset);

            // Run final demands through mixer to get motor values
            mixer.getMotors(demands, motorValues);
        }

}; // class HackflightForSim
