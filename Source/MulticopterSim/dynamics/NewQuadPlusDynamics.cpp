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

    protected:

        // Constants
        const double b(void)  override  { return 0.0000530216718361085; }
        const double d(void)  override  { return 2.23656692806239E-06; }
        const double m(void)  override  { return 16.47; }
        const double l(void)  override  { return 0.6; }
        const double Ix(void) override  { return 2; }
        const double Iy(void) override  { return 2; }
        const double Iz(void) override  { return 3; }
        const double Jr(void) override  { return 0.000308013; } // Kg*m^2

        const unsigned int maxrpm(void) override { return 10000; }

        // Eqn. 6 -------------------------------------------
        
        virtual double u2(double * o2) override
        {
            return o2[3] - o2[1];
        }

        virtual double u3(double * o2) override
        {
            return o2[2] - o2[0];
        }

        virtual double u4(double * o2) override
        {
            return o2[1] + o2[3] - o2[0] - o2[2];
        }

        virtual double omega(double * o)
        {
            return o[1] + o[3] - o[0] - o[2];
        }

    public:

        QuadPlusDynamics(void) : MultirotorDynamics(4) { }

}; // class QuadPlusDynamics

/*

// Factory method
MultirotorDynamics * MultirotorDynamics::create(void)
{
    return new QuadPlusDynamics();
}

*/

