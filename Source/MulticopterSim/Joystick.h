/*
 * Joystick.cpp: joystick/gamepad support for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "CoreMinimal.h"

/**
 * 
 */
class MULTICOPTERSIM_API Joystick {

    public:

        void init(void);

        void poll(int32_t axes[6], uint8_t & buttons);

        uint8_t axismap[5];
        uint8_t buttonmap[5];

        bool useButtonForAux = false;
        bool reversedVerticals = false;

    private:

        static const uint16_t VENDOR_STM	        = 0x0483;

        static const uint16_t PRODUCT_TARANIS		= 0x5710;
        static const uint16_t PRODUCT_PS3_CLONE		= 0x0003;
        static const uint16_t PRODUCT_XBOX360_CLONE	= 0xfafe;
        static const uint16_t PRODUCT_EXTREMEPRO3D	= 0xc215;
        static const uint16_t PRODUCT_F310	        = 0xc21d;
        static const uint16_t PRODUCT_PS4	        = 0x09cc;

        int  _id;

};
