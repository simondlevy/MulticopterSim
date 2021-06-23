/*
   Hackflight Motor class implementations for MulticopterSim

   Copyright(C) 2021 Simon D.Levy

   MIT License
   */

#pragma once

#include <rft_motors/rotary.hpp>

class SimMotor {

   protected:

        float _value = 0;

   public:

       float getValue(void) 
       {
           return _value;
       }

}; // class SimMotor

class SimRotaryMotor : public SimMotor, public rft::RotaryMotor {

    protected:

        virtual void write(float value) override 
        {
            _value = value;
        }

    public:

        SimRotaryMotor(void)
            : RotaryMotor(0) // dummy number for pin
        {
        }

}; // class SimRotaryMotor
