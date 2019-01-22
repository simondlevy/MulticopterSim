/*
* HackflightSimController.h: Abstract flight-control class for HackflightSim
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
class HACKFLIGHTSIM_API HackflightSimController {

public:

	HackflightSimController(void);

	~HackflightSimController(void);

    void update(void);

    static HackflightSimController * createController(void);
};
