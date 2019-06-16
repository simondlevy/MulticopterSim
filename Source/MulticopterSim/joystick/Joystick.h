/*
 * Joystick/gamepad support for flight simulators
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#pragma once

class Joystick {

    private:

        static const uint16_t PRODUCT_PS3_CLONE				= 0x0003;
        static const uint16_t PRODUCT_XBOX360				= 0x02a1;
		static const uint16_t PRODUCT_XBOX360_CLONE2		= 0x028e;
        static const uint16_t PRODUCT_XBOX360_WIRELESS		= 0x0719;
		static const uint16_t PRODUCT_REALFLIGHT_INTERLINK	= 0x0e56;
        static const uint16_t PRODUCT_TARANIS				= 0x5710;
        static const uint16_t PRODUCT_SPEKTRUM				= 0x572b;
        static const uint16_t PRODUCT_EXTREMEPRO3D			= 0xc215;
        static const uint16_t PRODUCT_F310					= 0xc216;
        static const uint16_t PRODUCT_PS4					= 0x09cc;
        static const uint16_t PRODUCT_XBOX360_CLONE			= 0xfafe;

        uint16_t _productId = 0;

        int _joystickId = 0;

        bool _isRcTransmitter = false;

        bool _inGimbalMode = false;

        // handles failure to calibrate transmitter before run
        void rescaleAxis(float & value, float minval, float maxval);

    public:

        typedef enum {

            ERROR_NOERROR,
            ERROR_MISSING,
            ERROR_PRODUCT

        } error_t;

        MULTICOPTERSIM_API Joystick(void);

        MULTICOPTERSIM_API error_t poll(float axes[6], uint8_t & buttonState);

        MULTICOPTERSIM_API bool isRcTransmitter(void);

        MULTICOPTERSIM_API bool inGimbalMode(void);
};
