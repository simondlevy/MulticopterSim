/*
 * Joystick.cpp: joystick/gamepad support for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "CoreMinimal.h"

#ifndef _WIN32
typedef uint32_t DWORD;
#endif

/**
 * 
 */
class MULTICOPTERSIM_API Joystick {

    public:

        bool isRcTransmitter;

        virtual void poll(float axes[6], uint8_t & buttonState) { (void)axes; (void)buttonState; }

        static Joystick * createJoystick(void);
};
