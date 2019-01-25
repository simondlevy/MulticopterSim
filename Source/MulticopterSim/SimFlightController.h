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

    virtual void init(void) = 0;

    virtual void update(float quat[4], float gyro[3], float motorvals[4]) = 0;

    static SimFlightController * createSimFlightController(void);
};
