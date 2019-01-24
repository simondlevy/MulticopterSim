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
class HACKFLIGHTSIM_API SimFlightController {

public:

	SimFlightController(void);

	~SimFlightController(void);

    void update(void);

    static SimFlightController * createSimFlightController(void);
};
