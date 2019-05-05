/*
 * Joystick.cpp: joystick/gamepad support for MulticopterSim
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
        static const uint16_t PRODUCT_XBOX360_WIRELESS		= 0x0719;
		static const uint16_t PRODUCT_REALFLIGHT_INTERLINK	= 0x0e56;
        static const uint16_t PRODUCT_TARANIS				= 0x5710;
        static const uint16_t PRODUCT_SPEKTRUM				= 0x572b;
        static const uint16_t PRODUCT_EXTREMEPRO3D			= 0xc215;
        static const uint16_t PRODUCT_F310					= 0xc216;
        static const uint16_t PRODUCT_PS4					= 0x09cc;
        static const uint16_t PRODUCT_XBOX360_CLONE			= 0xfafe;

        uint16_t _productId;

        int _joystickId;

    public:

        Joystick(void);

        bool isRcTransmitter;

        void poll(float axes[6], uint8_t & buttonState);
};
