/*
* Frame specification for a big-ass quadcopter
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "QuadXAPFrame.hpp"

class BigQuadFrame: public QuadXAPFrame {

    private:

        // Reverse-engineered so that sqrt(x^2+y^2) = 0.6
        static constexpr double MOTOR_LOCATIONS[12] = 
        {
            // X     Y    Z
            -.42, +.42, +.1, 
            +.42, +.42, +.1, 
            -.42, -.42, +.1, 
            +.42, -.42, +.1 
        };

    public:

        BigQuadFrame(void) 
            : QuadXAPFrame(MOTOR_LOCATIONS)
        {
        }

        virtual double b(void)  override { return 5.30216718361085E-05; }
        virtual double d(void)  override { return 2.23656692806239E-06; }
        virtual double m(void)  override { return 16.47; }
        virtual double Ix(void) override { return 2; }
        virtual double Iy(void) override { return 2; }
        virtual double Iz(void) override { return 3; }
        virtual double Jr(void) override { return 3.08013E-04; }

        virtual uint16_t maxrpm(void) override { return 15000; }

}; // class BigQuadFrame
