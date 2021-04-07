/*
   TargetManager implementation 

   Follows a Lorenz attractor just for fun

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/TargetManager.hpp"

class FLorenzTargetManager : public FTargetManager {

    private:

        const uint16_t COLS = 346;
        const uint16_t ROWS = 260;

        static constexpr float S = 10;
        static constexpr float R = 28;
        static constexpr float B = 2.667;

        // meters
        static constexpr float X_OFFSET = 20;
        static constexpr float Y_OFFSET =  0;
        static constexpr float Z_OFFSET =  5;

        static constexpr float SLOWDOWN  = 3;
        static constexpr float SCALEDOWN = 5;

        float _x = 0;
        float _y = 0;
        float _z = 0;

        double _previousTime = 0;

    public:

        FLorenzTargetManager(void)
        {
            _x = 0;
            _y = 1;
            _z = 1.05;

            _previousTime = 0;
        }

        virtual void computePose(double currentTime) override
        {
            double dt = (currentTime - _previousTime) / SLOWDOWN;

            _previousTime = currentTime;

            float xdot = S * (_y - _x);
            float ydot = R*_x - _y - _x*_z;
            float zdot = _x*_y - B*_z;

            _x += dt * xdot;
            _y += dt * ydot;
            _z += dt * zdot;

            _location.X = _x / SCALEDOWN + X_OFFSET;
            _location.Y = _y / SCALEDOWN + Y_OFFSET;
            _location.Z = _z / SCALEDOWN + Z_OFFSET;

            // Pause briefly to yield to other threads
            FPlatformProcess::Sleep(0.001);
        }
}; 
