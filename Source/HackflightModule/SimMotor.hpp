/*
   Hackflight Motor class implementations for MulticopterSim

   Copyright(C) 2021 Simon D.Levy

   MIT License
   */

#pragma once

#include <rft_motors/rotary.hpp>

class SimMotor {

   public:

       virtual float getValue(void) = 0;

}; // class SimMotor

class SimRotaryMotor : public SimMotor, public rft::RotaryMotor {

    private:

        float _value = 0;

    protected:

        virtual void write(float value) override 
        {
            _value = value;
        }

    public:

        SimRotaryMotor(void)
            : RotaryMotor(0) // dummy number for pin
        {
            _value = 0;
        }

        virtual float getValue(void) override
        {
            return _value;
        }

}; // class SimRotaryMotor
