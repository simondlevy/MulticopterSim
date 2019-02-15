/*
 * SimReceiver.h : Support USB controller for flight simulators
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#include "newreceiver.hpp"

#include "Joystick.h"

#pragma once

class SimReceiver : public hf::NewReceiver {

    public:

        SimReceiver(void);

}; // class SimReceiver
