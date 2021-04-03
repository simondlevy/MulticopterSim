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

		Joystick * _joystick = NULL;

        APlayerController * _playerController = NULL;

		// Helps mock up periodic availability of new data frame (output data rate; ODR)
		double _deltaT;
		double _previousTime;

        void checkKeypadKey(void)
        {
            if (hitEitherKey(EKeys::Nine, EKeys::NumPadNine)) {
                rft::Debugger::printf("THROTTLE UP");
            }

            if (hitEitherKey(EKeys::Three, EKeys::NumPadThree)) {
                rft::Debugger::printf("THROTTLE DOWN");
            }

            if (hitEitherKey(EKeys::Six, EKeys::NumPadSix)) {
                rft::Debugger::printf("ROLL RIGHT");
            }

            if (hitEitherKey(EKeys::Four, EKeys::NumPadFour)) {
                rft::Debugger::printf("ROLL LEFT");
            }

            if (hitEitherKey(EKeys::Eight, EKeys::NumPadEight)) {
                rft::Debugger::printf("PITCH FORWARD");
            }

            if (hitEitherKey(EKeys::Two, EKeys::NumPadTwo)) {
                rft::Debugger::printf("PITCH BACK");
            }

            if (hitKey(EKeys::Enter)) {
                rft::Debugger::printf("YAW RIGHT");
            }

            if (hitEitherKey(EKeys::Zero, EKeys::NumPadZero)) {
                rft::Debugger::printf("YAW LEFT");
            }

            if (hitEitherKey(EKeys::Five, EKeys::NumPadFive)) {
                rft::Debugger::printf("CENTER ALL");
            }
        }

        bool hitEitherKey(const FKey key1, const FKey key2)
        {
            return hitKey(key1) || hitKey(key2);
        }

        bool hitKey(const FKey key)
        {
            return _playerController->IsInputKeyDown(key);
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
            _playerController = playerController;

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

		void update(void)
		{
			// Joystick::poll() returns zero (okay) or a postive value (error)
			if (!_joystick->poll(rawvals)) return;

            checkKeypadKey();
		}

}; // class SimReceiver
