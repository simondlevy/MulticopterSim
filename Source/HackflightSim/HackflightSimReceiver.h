/*
* HackflightSimReceiver.h : Support USB controller for flight simulators
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>

#include <receiver.hpp>

namespace hf {

    class Controller : public Receiver {

        public:

            bool arming(void) override
            {
                // Return true first time around only
                bool retval = (_mode==MODE_LOITER) ? demands.throttle > STICK_DEADBAND : true;

                // Don't report arming if already armed
                if (_armed) {
                    retval = false;
                }

                // On first arming, set already-armed flag
                else if (retval) {
					_armed = true;
                }

                return retval;
            }

            // Once armed, sim never disarms
            bool disarming(void) override
            {
                return false;
            }

            Controller(void)
            {
                _reversedVerticals = false;
                _springyThrottle = false;
                _useButtonForAux = false;
                _joyid = 0;
                _cycle = 0;

                _buttonState = 0;
            }

            void begin(void)
            {
                // Initialize flag for arming
                _armed = false;

                // Set up axes based on OS and controller
                productInit();
            }

            bool gotNewFrame(void)
            {
                return (++_cycle % 3) ? false : true;
            }

            void readRawvals(void)
            {
                static int32_t axes[6];
                static uint8_t buttons;

                // Grab the axis values in an OS-specific way
                productPoll(axes, buttons);

                // Normalize the axes to demands in [-1,+1]
                for (uint8_t k=0; k<5; ++k) {
                    rawvals[k] = (axes[_axismap[k]] - productGetBaseline()) / 32767.f;
                }

                // Invert throttle, pitch if indicated
                if (_reversedVerticals) {
                    rawvals[0] = -rawvals[0];
                    rawvals[2] = -rawvals[2];
                }

                // For game controllers, use buttons to fake up values in a three-position aux switch
                if (_useButtonForAux) {
                    for (uint8_t k=0; k<3; ++k) {
                        if (buttons == _buttonmap[k]) {
                            _buttonState = k;
                        }
                    }
                    rawvals[4] = buttonsToAux[_buttonState];
                }
            }

            void halt(void)
            {
            }

		protected:

			virtual flightmode_t flightMode(void) override
			{
				return _springyThrottle ? MODE_LOITER : _mode;
			}

        private:

            // A hack to support arming on startup
            bool     _armed;

            // Implemented differently for each OS
            void     productInit(void);
            void     productPoll(int32_t axes[6], uint8_t & buttons);
            int32_t  productGetBaseline(void);

            // Determined dynamically based on controller
            bool     _reversedVerticals;
            bool     _springyThrottle;
            bool     _useButtonForAux;
            uint8_t  _axismap[5];   // Thr, Ael, Ele, Rud, Aux
            uint8_t  _buttonmap[3]; // Aux=0, Aux=1, Aux=2
            int      _joyid;        // Linux file descriptor or Windows joystick ID

            // Simulate auxiliary switch via pushbuttons
            uint8_t _buttonState;
            const float buttonsToAux[3] = {-.1f, 0.f, .8f};

            // Helps mock up periodic availability of new data frame (output data rate; ODR)
            uint64_t _cycle;          

    }; // class Controller

} // namespace hf
