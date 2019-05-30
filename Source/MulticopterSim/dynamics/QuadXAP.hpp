/*
 * Frame methods implementation for a X-confuration quadcopter using Ardupilot layout:
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

// Eqn. 6 -------------------------------------------

// thrust roll right
static double quadxap_u2(double * o2)
{
    return (o2[1] + o2[2]) - (o2[0] + o2[3]);
}

// thrust pitch forward
static double quadxap_u3(double * o2)
{
    return (o2[1] + o2[3]) - (o2[0] + o2[2]);
}

// thrust yaw cw
static double quadxap_u4(double * o2)
{
    return (o2[0] + o2[1]) - (o2[2] + o2[3]);
}

// torque cw
static double quadxap_omega(double * o)
{
    return (o[0] + o[1]) - (o[2] + o[3]);
}

static MultirotorDynamics::frame_t frame = 
{
    4,
    quadxap_u2,
    quadxap_u3,
    quadxap_u4,
    quadxap_omega
};
