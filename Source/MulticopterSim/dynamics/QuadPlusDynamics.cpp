/*
 * MultirotorDynamics implementation for + confuration quadcopter:
 *
 *         1cw
 *          |
 *          |
 *   4ccw - + - 2ccw
 *          |
 *          |
 *         3cw
 *
 * See: https://charlestytler.com/modeling-vehicle-dynamics-6dof-nonlinear-simulation/#Equations-of-Motion
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "MultirotorDynamicsNew.hpp"

class QuadPlusDynamics : public MultirotorDynamics {

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
        const double B    = 0.000005; 
        const double Bnew = 0.0000530216718361085;
        const double Dnew = 2.23656692806239E-06;

        // Current motor values in interval [0,1]
        double _motorvals[4] = {0};

    protected:

        virtual void getForces(double & U1, double & U2, double & U3, double & U4) override
        {
            double omega21 = rpss(_motorvals[0], MAXRPM);
            double omega22 = rpss(_motorvals[1], MAXRPM);
            double omega23 = rpss(_motorvals[2], MAXRPM);
            double omega24 = rpss(_motorvals[3], MAXRPM);

            U1 = Bnew * (omega21 + omega22 + omega23 + omega24);
            U2 = Bnew * (omega24 + omega22);
            U3 = Bnew * (omega23 - omega21);
        }

    public:

        void setMotors(double * motorvals)
        {
            for (uint8_t k=0; k<4; ++k) {
                _motorvals[k] = motorvals[k];
            }
        }

}; // class QuadPlusDynamics

// Factory method
/*
MultirotorDynamics * MultirotorDynamics::create(void)
{
    return new QuadPlusDynamics();
}
*/

