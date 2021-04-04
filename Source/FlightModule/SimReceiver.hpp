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

        static constexpr float KEY_STEP = .001;

        // We use a joystick (game controller) if one is available
		Joystick * _joystick = NULL;

        // Otherwise, use use the numeric keypad
        APlayerController * _playerController = NULL;

		// Helps mock up periodic availability of new data frame (output data rate; ODR)
		double _deltaT;
		double _previousTime;

        bool hitEitherKey(const FKey key1, const FKey key2)
        {
            return hitKey(key1) || hitKey(key2);
        }

        bool hitKey(const FKey key)
        {
            return _playerController->IsInputKeyDown(key);
        }

        static const float max(float a, float b)
        {
            return a > b ? a : b;
        }

        static const float min(float a, float b)
        {
            return a < b ? a : b;
        }


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

		SimReceiver(APlayerController * playerController, uint16_t updateFrequency=50)
			: Receiver(DEFAULT_CHANNEL_MAP, DEMAND_SCALE)
		{
			_joystick = new Joystick();

            _playerController = playerController;

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
			return _joystick->poll(rawvals);

 		}

        static void constrain(float & value, int8_t inc)
        {
            value += inc * KEY_STEP;

            value = value > +1 ? +1 : (value < -1 ? -1 : value);
        }

        void tick(void)
        {
            if (hitEitherKey(EKeys::Nine, EKeys::NumPadNine)) {
                constrain(rawvals[0], +1);
            }

            if (hitEitherKey(EKeys::Three, EKeys::NumPadThree)) {
                constrain(rawvals[0], -1);
            }

            if (hitEitherKey(EKeys::Six, EKeys::NumPadSix)) {
                constrain(rawvals[1], +1);
            }

            if (hitEitherKey(EKeys::Four, EKeys::NumPadFour)) {
                constrain(rawvals[1], -1);
            }

            if (hitEitherKey(EKeys::Eight, EKeys::NumPadEight)) {
                constrain(rawvals[2], +1);
            }

            if (hitEitherKey(EKeys::Two, EKeys::NumPadTwo)) {
                constrain(rawvals[2], -1);
            }

            if (hitKey(EKeys::Enter)) {
                constrain(rawvals[3], +1);
            }

            if (hitEitherKey(EKeys::Zero, EKeys::NumPadZero)) {
                constrain(rawvals[3], -1);
            }

            if (hitEitherKey(EKeys::Five, EKeys::NumPadFive)) {
                rawvals[0] = 0;
                rawvals[1] = 0;
                rawvals[2] = 0;
                rawvals[3] = 0;
            }
        }

}; // class SimReceiver
