/*
 * Joystick/gamepad support for flight simulators
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include <stdint.h>
#include <stdbool.h>

#ifndef MULTICOPTERSIM_API
#define MULTICOPTERSIM_API
#endif

class Joystick {

    private:

        static const uint16_t PRODUCT_PS3_CLONE			    = 0x0003;
        static const uint16_t PRODUCT_XBOX360			    = 0x02a1;
        static const uint16_t PRODUCT_XBOX360_CLONE2		= 0x028e;
        static const uint16_t PRODUCT_XBOX360_WIRELESS		= 0x0719;
        static const uint16_t PRODUCT_INTERLINK	            = 0x0e56;
        static const uint16_t PRODUCT_TARANIS			    = 0x5710;
        static const uint16_t PRODUCT_SPEKTRUM			    = 0x572b;
        static const uint16_t PRODUCT_EXTREMEPRO3D		    = 0xc215;
        static const uint16_t PRODUCT_F310			        = 0xc216;
        static const uint16_t PRODUCT_PS4			        = 0x09cc;
        static const uint16_t PRODUCT_XBOX360_CLONE		    = 0xfafe;

        static constexpr float AUX1_MID = 0.3f; // positve but less than 0.5

        uint16_t _productId = 0;

        int _joystickId = 0;

        bool _isRcTransmitter = false;

        bool _inGimbalMode = false;

        // handles failure to calibrate transmitter before run
        void rescaleAxis(float & value, float minval, float maxval);

        void buttonsToAxes(uint8_t number, uint16_t value, float * axes);
        void buttonsToAxesGamepad(uint8_t number, uint16_t value, float * axes, uint8_t top, uint8_t rgt, uint8_t bot, uint8_t lft);
        void buttonsToAxesInterlink(uint8_t number, uint16_t value, float * axes);

    public:

        typedef enum {

            ERROR_NOERROR,
            ERROR_MISSING,
            ERROR_PRODUCT

        } error_t;

        MULTICOPTERSIM_API Joystick(const char * devname="/dev/input/js0"); // ignored by Windows

        MULTICOPTERSIM_API error_t poll(float axes[6], uint8_t & buttonState);

        MULTICOPTERSIM_API bool isRcTransmitter(void)
        {
            return _isRcTransmitter;
        }

        MULTICOPTERSIM_API bool inGimbalMode(void)
        {
            return _inGimbalMode;
        }
};
