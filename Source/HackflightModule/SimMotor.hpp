/*
   RFT Motor class implementations for MulticopterSim

   Acts as a pass-through for values set by the mixer

   Copyright(C) 2021 Simon D.Levy

   MIT License
   */

#pragma once

#include <hf_motors/rotary.hpp>
#include <hf_motors/servo.hpp>

class SimMotor {

   protected:

        float _value = 0;

   public:

       float getValue(void) 
       {
           return _value;
       }

}; // class SimMotor


class SimRotaryMotor : public SimMotor, public hf::RotaryMotor {

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


class SimServoMotor : public SimMotor, public hf::ServoMotor {

    protected:

        virtual void write(float value) override 
        {
            _value = value;
        }

    public:

        SimServoMotor(void)
            : ServoMotor(0) // dummy number for pin
        {
        }

}; // class SimServoMotor
