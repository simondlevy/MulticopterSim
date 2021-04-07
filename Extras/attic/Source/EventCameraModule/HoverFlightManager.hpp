/*
   MulticopterSim FlightManager class implementation that just does takeoff and hover

   Rises to a few meters then cuts motors

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/FlightManager.hpp"

class FHoverFlightManager : public FFlightManager {

    private:


        // Target
        static constexpr double ALTITUDE_TARGET = 10;

        // PID params
        static constexpr double ALT_P = 1.0;
        static constexpr double VEL_P = 1.0;

        // Velocity tracking
        double _zprev = 0;
        double _tprev = 0;

        // Utilities
        static double min(double a, double b) { return a < b ? a : b; }
        static double max(double a, double b) { return a > b ? a : b; }

    public:

        // Constructor
        FHoverFlightManager(MultirotorDynamics * dynamics) 
            : FFlightManager(dynamics) 
        {
            _zprev = 0;
            _tprev = 0;
        }

        virtual ~FHoverFlightManager(void)
        {
        }

        virtual void getMotors(const double time, const MultirotorDynamics::state_t & state, double * motorvals) override
        {
            double z = -state.pose.location[2]; // NED

            // Compute velocity target in proportion to altitude error
            double velTarget = (ALTITUDE_TARGET - z) * ALT_P;

            // Velocity error will be zero on first iteration
            double velError = 0;

            // Computer velocity error after first iteration
            if (_tprev > 0) {
                double dt = time - _tprev;
                double vel = (z-_zprev) / dt;
                velError = velTarget - vel;
            }

            // Store time and altitude for velocity tracking
            _tprev = time;
            _zprev = z;

            // Compute motor demand using velocity PID control
            double u = VEL_P * velError;

            // Constrain motor demand to [0,1]
            u = min(max(u, 0), 1);

            // Wait a bit to yield to other threads
            FPlatformProcess::Sleep(.001);

            // Set all motors to same demand value
            for (uint8_t i=0; i<_motorCount; ++i) {
                motorvals[i] = u;
            }

        }

}; // HoverFlightManager
