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

    virtual void init(uint8_t  axismap[5], uint8_t buttonmap[3], bool reversedVerticals, bool springyThrottle, bool useButtonForAux) = 0;

    virtual void update(int32_t axes[6], uint8_t buttons, float quat[4], float gyro[3]) = 0;

    static SimFlightController * createSimFlightController(void);
};
