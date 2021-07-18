/*
 * General support for joysticks and other game controllers
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */


#pragma once

#include <CoreMinimal.h>
#include <UObject/Interface.h>
#include "Joystick.generated.h"

// This class does not need to be modified.
UINTERFACE(MinimalAPI)
class UJoystick : public UInterface
{
	GENERATED_BODY()
};

/**
 * 
 */
class IJoystick
{
	GENERATED_BODY()

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

        // XXX Should use a separate calibration program
        static void adjustAxesInterlink(float * axes)
        {
            axes[0] /= 0.575f;
            axes[1] /= 0.65f;
            axes[2] /= 0.58f;
            axes[3] /= 0.65f;
        }

        static void getAxes4(float axes[6], DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3)
        {
            axes[0] = (float)axis0;
            axes[1] = (float)axis1;
            axes[2] = (float)axis2;
            axes[3] = (float)axis3;
        }

        static void getAxes5(float axes[6], uint8_t & naxes, DWORD axis0, DWORD axis1, DWORD axis2, DWORD axis3, DWORD axis4)
        {
            naxes = 5;
            getAxes4(axes, axis0, axis1, axis2, axis3);
            axes[4] = (float)axis4;
        }

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

        // Convert InterLink aux switches to unique gamepad buttons
        static void getAuxInterlink(float * axes, uint8_t buttons, uint8_t aux1, uint8_t aux2, float auxMid)
        {
            axes[aux1] = -1;
            axes[aux2] = (buttons & 0x01) ? -1.f : +1.f;

            switch (buttons) {

            case 3:
            case 2:
                axes[aux1] = auxMid;
                break;

            case 19:
            case 18:
                axes[aux1] = 1;
            }
        }

        void pollProduct(float axes[6], uint8_t & buttons);

        static bool isValidJoystick(int joystick_id, uint16_t & product_id);

    public:

        IJoystick(void)
        {
            _isGameController = false;

            // Grab the first available joystick
            for (_joystickId=0; _joystickId<16; _joystickId++) {
                if (isValidJoystick(_joystickId, _productId)) {
                    break;
                }
            }

            if (_joystickId < 16) {

                switch (_productId) {
                    case PRODUCT_TARANIS_QX7:
                    case PRODUCT_TARANIS_X9D:
                    case PRODUCT_SPEKTRUM:
                        _isGameController = false;
                        break;
                    default: 
                        _isGameController = true;
                }
            }
        }

        void poll(float axes[6])
        {
            uint8_t buttons = 0;

            pollProduct(axes, buttons);

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
        }
};
