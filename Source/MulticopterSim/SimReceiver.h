/*
 * SimReceiver.h : Support USB controller for flight simulators
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

    class SimReceiver : public Receiver {

        public:

            SimReceiver(uint8_t  _axismap[5], uint8_t buttonmap[3], bool reversedVerticals, bool springyThrottle, bool useButtonForAux);

            void begin(void);

            bool gotNewFrame(void);

            void readRawvals(void);

            void halt(void);

            void update(int32_t axes[6], uint8_t buttons);

        protected:

            virtual uint8_t getAux1State(void) override;

            virtual uint8_t getAux2State(void) override;

            //virtual void productInit(void) = 0;
            //virtual void productPoll(int32_t axes[6], uint8_t & buttons) = 0;

            // Determined dynamically based on controller
            uint8_t  _axismap[5];   // Thr, Ael, Ele, Rud, Aux
            uint8_t  _buttonmap[3]; // Aux=0, Aux=1, Aux=2
            bool     _reversedVerticals;
            bool     _springyThrottle;
            bool     _useButtonForAux;

            // Simulate auxiliary switch via pushbuttons
            uint8_t _buttonState;
            const float buttonsToAux[3] = {-.1f, 0.f, .8f};

            // Helps mock up periodic availability of new data frame (output data rate; ODR)
            uint64_t _cycle;          


        private:

            int32_t _axes[6];
            uint8_t _buttons;

    }; // class SimReceiver

} // namespace hf
