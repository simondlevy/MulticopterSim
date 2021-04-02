/*
   Hackflight Receiver subclass for MulticopterSim Allows us to treat an input
   device (joystick, game controller, R/C transmitter) as a "virtual receiver"
   for the firmware.

   Copyright(C) 2019 Simon D.Levy

   MIT License
   */

#pragma once

#include <receiver.hpp>
#include <RFT_debugger.hpp>

#include "../joystick/Joystick.hpp"

class SimReceiver : public hf::Receiver {

    friend class FHackflightManager;

    private:

		static constexpr uint8_t DEFAULT_CHANNEL_MAP[6] = { 0, 1, 2, 3, 4, 5 };
		static constexpr float DEMAND_SCALE = 1.0f;

		Joystick * _joystick;

		// Helps mock up periodic availability of new data frame (output data rate; ODR)
		double _deltaT;
		double _previousTime;

    protected:

		virtual bool inArmedState(void) override
		{
			return true; // Always armed
		}

		virtual uint8_t getModeIndex(void) override
		{
			return 0; // XXX Receiver::getAux1State();  // With only five channels, we use Aux1 for Aux2
		}

    public:

		SimReceiver(uint16_t updateFrequency=50)
			: Receiver(DEFAULT_CHANNEL_MAP, DEMAND_SCALE)
		{
			_joystick = new Joystick();

			_deltaT = 1./updateFrequency;
			_previousTime = 0;
		}

		void begin(void)
		{
		}

		bool gotNewFrame(void)
		{
			// Get a high-fidelity current time value from the OS
			double currentTime = FPlatformTime::Seconds();

			if (currentTime-_previousTime > _deltaT) {
				_previousTime = currentTime;
				static uint32_t count;
				return true;
			}

			return false;
		}

		void readRawvals(void)
		{
		}

		uint16_t update(void)
		{
			// Joystick::poll() returns zero (okay) or a postive value (error)
			uint16_t result = _joystick->poll(rawvals);

            if (result == 0) {
                return 0;
            }

            return 0; //result;
		}

}; // class SimReceiver
