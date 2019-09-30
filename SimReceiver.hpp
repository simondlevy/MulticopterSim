/*
   Hackflight Receiver subclass for MulticopterSim Allows us to treat an input
   device (joystick, game controller, R/C transmitter) as a "virtual receiver"
   for the firmware.

   Copyright(C) 2019 Simon D.Levy

   MIT License
   */

#pragma once

#include <receiver.hpp>

#include "../MainModule/joystick/Joystick.h"

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

		uint8_t getAux1State(void) 
		{
			return Receiver::getAux1State();
		}

		uint8_t getAux2State(void)
		{
			// Always armed!
			return 1;
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
				return true;
			}

			return false;
		}

		void readRawvals(void)
		{
		}

		Joystick::error_t update(void)
		{
			// Joystick::poll() returns zero (okay) or a postive value (error)
			return _joystick->poll(rawvals);
		}

}; // class SimReceiver
