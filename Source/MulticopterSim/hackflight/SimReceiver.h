/*
 * SimReceiver.h : Support USB controller for flight simulators
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include <receiver.hpp>

#include "Joystick.h"

namespace hf {

    class SimReceiver : public Receiver {

        public:

            SimReceiver(void);

            void begin(void);

            bool gotNewFrame(void);

            void readRawvals(void);

            void update(void);

        protected:

            virtual uint8_t getAux1State(void) override;

            virtual uint8_t getAux2State(void) override;

            // Simulate auxiliary switch via pushbuttons
            uint8_t _buttonState;
            const float buttonsToAux[3] = {-.1f, 0.f, .8f};

            // Helps mock up periodic availability of new data frame (output data rate; ODR)
            uint64_t _cycle;          

        private:

            Joystick joystick;

    }; // class SimReceiver

} // namespace hf
