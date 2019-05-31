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

// roll right
static double quadxap_u2(double * o)
{
    return (o[1] + o[2]) - (o[0] + o[3]);
}

// pitch forward
static double quadxap_u3(double * o)
{
    return (o[1] + o[3]) - (o[0] + o[2]);
}

// yaw cw
static double quadxap_u4(double * o)
{
    return (o[0] + o[1]) - (o[2] + o[3]);
}

static MultirotorDynamics::frame_t frame = 
{
    4,
    quadxap_u2,
    quadxap_u3,
    quadxap_u4
};
