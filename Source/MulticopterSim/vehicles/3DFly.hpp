/*
 * Frame implementation for ThreeDFly
 *
 * For reference citation see MultirotorDynamics.hpp
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "QuadXAPFrame.hpp"

class ThreeDFlyFrame: public QuadXAPFrame {

    private:

        static constexpr double MOTOR_LOCATIONS[12] = 
        {
            -.0375,+.037,+.01,
            +.0375,+.037,+.01,
            -.0375,-.037,+.01,
            +.0375,-.037,+.01 
        };

    public:

        ThreeDFlyFrame(void) 
            : QuadXAPFrame(MOTOR_LOCATIONS)
        {
        }

        // These values are estimated so as to give reasonable behavior
        virtual double b(void)  override { return  5E-08; }
        virtual double d(void)  override { return  2E-06; }
        virtual double m(void)  override { return  0.1; }
        virtual double Ix(void)  override { return  .2; }
        virtual double Iy(void)  override { return  .2; }
        virtual double Iz(void)  override { return  .3; }
        virtual double Jr(void)  override { return  3E-4; }
        virtual uint16_t maxrpm(void)  override { return  40000; }

}; // class ThreeDFlyFrame

