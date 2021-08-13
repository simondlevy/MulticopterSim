/*
   Hackflight Receiver subclass for MulticopterSim Allows us to treat an input
   device (joystick, game controller, R/C transmitter) as a "virtual receiver"
   for the firmware.

   Copyright(C) 2019 Simon D.Levy

   MIT License
   */

#pragma once

#include <hf_receiver.hpp>
#include <RFT_debugger.hpp>

#include "../MainModule/GameInput.hpp"

class SimReceiver : public hf::Receiver {

    private:

		static constexpr uint8_t DEFAULT_CHANNEL_MAP[6] = { 0, 1, 2, 3, 4, 5 };
		static constexpr float DEMAND_SCALE = 1.0f;

        // Joystick (RC transmitter, game controller) or keyboard
        GameInput * _gameInput = NULL;

        double _joyvals[4] = {};


    protected:

		virtual bool inArmedState(void) override
		{
			return true; // Always armed
		}

		virtual uint8_t getModeIndex(void) override
		{
			// With only five channels, we use Aux1 for Aux2
            return 0; // XXX Receiver::getAux1State();  
		}

   		bool gotNewFrame(void) override
		{
			return false;
		}

        virtual void readRawvals(void) override
        {
        }

        // Sim works with double-precision floats, so we need to
        // copy demands to floating-point values for Hackflight
        void d2f()
        {
        }

    public:

        SimReceiver(APawn * pawn, uint16_t updateFrequency=50)
            : Receiver(DEFAULT_CHANNEL_MAP, DEMAND_SCALE)
        {
            _gameInput = new GameInput(pawn);
		}

		void poll(void)
		{
		    _gameInput->getJoystick(_joyvals);
            //d2f();

        }

        void tick(void)
        {
            //_gameInput->getKeypad(joyvals);
            //d2f();
        }

}; // class SimReceiver
