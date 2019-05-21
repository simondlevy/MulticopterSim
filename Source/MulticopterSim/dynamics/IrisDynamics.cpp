/*
 * MultirotorDynamics implementation for 3DR Iris
 *
 *
 * Uses CLeanflight QuadX motor layout:
 *
 *   3   1
 *     X
 *   2   4
 *
 * See: https://charlestytler.com/modeling-vehicle-dynamics-6dof-nonlinear-simulation/#Equations-of-Motion
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "MultirotorDynamics.hpp"

class IrisDynamics : public MultirotorDynamics {

    private:

        // Motor distances from center of mass, in meters
        const double d1x = 0.150;
        const double d1y = 0.240;
        const double d2x = 0.150;
        const double d2y = 0.225;
        const double d3x = 0.150;
        const double d3y = 0.240;
        const double d4x = 0.150;
        const double d4y = 0.225;

        // Motor constants
        double MAXRPM = 10000;

        // Propeller constants
        const double B = 0.000005;

        // Current motor values in interval [0,1]
        double _motorvals[4] = {0};

    protected:

        // Get forces based on current motor values.
        virtual void getForces(double & Fz, double & L, double & M, double & N) override
        {
            // Convert motor values in [0,1] to thrusts in Newtons
            double F1 = Fthrust(_motorvals[0], B, MAXRPM);
            double F2 = Fthrust(_motorvals[1], B, MAXRPM);
            double F3 = Fthrust(_motorvals[2], B, MAXRPM);
            double F4 = Fthrust(_motorvals[3], B, MAXRPM);

            // Convert motor thrusts to angular accelerations
            L = (F2*d2y + F3*d3y) - (F1*d1y + F4*d4y);
            M = (F1*d1x + F3*d3x) - (F2*d2x + F4*d4x); 
            N = (T(F1,d1x,d1y) + T(F2,d2x,d2y)) - (T(F3,d3x,d3y) + T(F4,d4x,d4y)); 

            // Compute orthogonal force component Fz
            Fz = F1 + F2 + F3 + F4;
        }

        virtual void getForces2(double & U1, double & U2, double & U3, double & U4) override
        {
            double omega21 = rpss(_motorvals[0], MAXRPM);
            double omega22 = rpss(_motorvals[1], MAXRPM);
            double omega23 = rpss(_motorvals[2], MAXRPM);
            double omega24 = rpss(_motorvals[3], MAXRPM);

            U1 = B * (omega21 + omega22 + omega23 + omega24);
        }

    public:

        void setMotors(double * motorvals)
        {
            for (uint8_t k=0; k<4; ++k) {
                _motorvals[k] = motorvals[k];
            }
        }

}; // class IrisDynamics

// Factory method
MultirotorDynamics * MultirotorDynamics::create(void)
{
    return new IrisDynamics();
}

