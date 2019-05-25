/*
 * MultirotorDynamics implementation for a X-confuration quadcopter using
 * Ardupilot layout:
 *
 *   3cw   1ccw
 *      \ /
 *       X
 *      / \
 *   2ccw  4cw
 *
 * For reference citation see MultirotorDynamics.hpp
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "MultirotorDynamics.hpp"

class QuadXAPDynamics : public MultirotorDynamics {

    protected:

        // Eqn. 6 -------------------------------------------
        
        // thrust roll right
        virtual double u2(double * o2) override
        {
            return (o2[1] + o2[2]) - (o2[0] + o2[3]);
        }

        // thrust pitch forward
        virtual double u3(double * o2) override
        {
            return (o2[1] + o2[3]) - (o2[0] + o2[2]);
        }

        // thrust yaw cw
        virtual double u4(double * o2) override
        {
            return (o2[0] + o2[1]) - (o2[2] + o2[3]);
        }

        // torque cw
        virtual double omega(double * o)
        {
            return (o[0] + o[1]) - (o[2] + o[3]);
        }

    public:

        QuadXAPDynamics(void) : MultirotorDynamics(4) { }

}; // class QuadXAPDynamics
