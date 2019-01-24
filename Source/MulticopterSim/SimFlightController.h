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

	SimFlightController(void);

	~SimFlightController(void);

    void init(uint8_t  axismap[5], uint8_t buttonmap[3], bool reversedVerticals, bool springyThrottle, bool useButtonForAux);

    void update(void);

    static SimFlightController * createSimFlightController(void);
};
