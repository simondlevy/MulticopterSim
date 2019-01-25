/*
* SimFlightController.h: Abstract flight-control class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#pragma once

#include "CoreMinimal.h"

/**
 * 
 */
class MULTICOPTERSIM_API SimFlightController {

public:

    /**
     *  Performs any necessary initialization for your subclass implementation.
     */
    virtual void init(void) = 0;

    /**
     * Updates the flight controller with the current quaternion and gyrometer,
     * outputting motor values.
     * @param quat current quaternion
     * @param gyro current gyrometer rates in radians / sec
     * @param motorvals motor values between 0 and 1 output by your implementation
     */
     virtual void update(float quat[4], float gyro[3], float motorvals[4]) = 0;

    /**
     *  Factory method.
     */
     static SimFlightController * createSimFlightController(void);
};
