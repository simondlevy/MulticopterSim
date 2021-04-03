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
#include <stdio.h>

class Joystick {

    private:

        static const uint16_t PRODUCT_XBOX_ONE         = 0x02ff;
        static const uint16_t PRODUCT_XBOX360          = 0x02a1;
        static const uint16_t PRODUCT_XBOX360_CLONE    = 0xfafe;
        static const uint16_t PRODUCT_XBOX360_CLONE2   = 0x028e;
        static const uint16_t PRODUCT_XBOX360_WIRELESS = 0x0719;
        static const uint16_t PRODUCT_TARANIS_QX7      = 0x5720;
        static const uint16_t PRODUCT_TARANIS_X9D      = 0x5710;
        static const uint16_t PRODUCT_PS3_CLONE        = 0x0003;
        static const uint16_t PRODUCT_INTERLINK        = 0x0e56;
        static const uint16_t PRODUCT_SPEKTRUM         = 0x572b;
        static const uint16_t PRODUCT_EXTREMEPRO3D     = 0xc215;
        static const uint16_t PRODUCT_F310             = 0xc216;
        static const uint16_t PRODUCT_PS4              = 0x09cc;

        static constexpr float AUX1_MID = 0.3f; // positve but less than 0.5

        uint16_t _productId = 0;

        int _joystickId = 0;

        bool _isGameController = false;

    protected:

        enum {

            AX_THR,
            AX_ROL,
            AX_PIT,
            AX_YAW,
            AX_AU1,
            AX_AU2,
            AX_NIL
        };

        void buttonsToAxes(uint8_t buttons, uint8_t top, uint8_t rgt, uint8_t bot, uint8_t lft, float * axes)
        {
            static float _aux1 = 0;
            static float _aux2 = -1;

            static bool _down;

            if (buttons) {

                if (!_down) {

                    // Left button sets AUX2
                    if (buttons == lft) {
                        _aux2 *= -1;
                    }

                    // Other buttons set AUX1
                    else {
                        _aux1 = (buttons == top) ? -1 : (buttons == rgt ? AUX1_MID : +1);
                    }

                    _down = true;
                }
            }

            else {
                _down = false;
            }

            axes[AX_AU1] = _aux1;
            axes[AX_AU2] = _aux2;
        }

        // Returns 0 for no error, 1 for missing, product ID if not recognized
        uint16_t pollProduct(float axes[6], uint8_t & buttons);

    public:

        Joystick(const char * devname = "/dev/input/js0"); // ignored by Windows

        uint16_t poll(float axes[6])
        {
            uint8_t buttons = 0;

            uint16_t status = pollProduct(axes, buttons);

            // Invert throttle, pitch axes on game controllers
            if (_isGameController) {
                axes[AX_THR] *= -1;
                axes[AX_PIT] *= -1;
            }

            switch (_productId) {

                case PRODUCT_F310:
                    buttonsToAxes(buttons, 8, 4, 2, 1, axes);
                    break;

                case PRODUCT_XBOX_ONE:
                case PRODUCT_XBOX360:
                case PRODUCT_XBOX360_CLONE:
                case PRODUCT_XBOX360_CLONE2:
                case PRODUCT_XBOX360_WIRELESS:
                    buttonsToAxes(buttons, 8, 2, 1, 4, axes);
            }

            return status;
        }
};
