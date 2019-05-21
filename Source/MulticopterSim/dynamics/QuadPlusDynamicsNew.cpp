/*
 * MultirotorDynamics implementation for a plus-confuration quadcopter:
 *
 *         1cw
 *          |
 *          |
 *   4ccw - + - 2ccw
 *          |
 *          |
 *         3cw
 *
 * For reference citation see MultirotorDynamics.hpp
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "NewMultirotorDynamics.hpp"

class QuadPlusDynamics : public MultirotorDynamics {

    private:

        // Motor constants
        double MAXRPM = 10000;

        // Current motor values in interval [0,1]
        double _motorvals[4] = {0};

    protected:

        // Propeller/motor constants
        const double b(void)  override  { return 0.0000530216718361085; }
        const double d(void)  override  { return 2.23656692806239E-06; }
        const double m(void)  override  { return 16.47; }
        const double l(void)  override  { return 0.6; }
        const double Ix(void) override  { return 2; }
        const double Iy(void) override  { return 2; }
        const double Iz(void) override  { return 3; }
        const double Jr(void) override  { return 0.000308013; } // Kg*m^2

        virtual void getForces(double & U1, double & U2, double & U3, double & U4, double & Omega) override
        {
            double o1 = rps(_motorvals[0], MAXRPM);
            double o2 = rps(_motorvals[1], MAXRPM);
            double o3 = rps(_motorvals[2], MAXRPM);
            double o4 = rps(_motorvals[3], MAXRPM);

            double o21 = o1 * o1;
            double o22 = o2 * o2;
            double o23 = o3 * o3;
            double o24 = o4 * o4;

            // Eqn. 6
            U1 = o21 + o22 + o23 + o24;
            U2 = o24 + o22;
            U3 = o23 - o21;
            U4 = o22 + o24 - o21 - o23;
            Omega = o2 + o4 - o1 - o3;
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

