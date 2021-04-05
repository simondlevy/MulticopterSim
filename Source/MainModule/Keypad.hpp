/*
   Numeric keypad control for flight axes

   Copyright(C) 2021 Simon D.Levy

   MIT License
   */

#pragma once

#include "CoreMinimal.h"

class Keypad {

    private:

        static constexpr float STEP = .001;

        float rawvals[4] = {};

        APlayerController * _playerController = NULL;

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


        static void constrain(float & value, int8_t inc)
        {
            value += inc * STEP;

            value = value > +1 ? +1 : (value < -1 ? -1 : value);
        }

    public:

		Keypad(APlayerController * playerController)
		{
            _playerController = playerController;
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

}; // class Keypad
