/*
   Hackflight Motor class implementation for MulticopterSim

   Copyright(C) 2020 Simon D.Levy

   MIT License
   */

#pragma once

#include <motor.hpp>

class SimMotor : public hf::Motor {

    private:

        float _value = 0;

    public:

        SimMotor(void)
            : Motor(0)
        {
        }

        float getValue(void)
        {
            return _value;
        }

    protected:

        virtual void write(float value) override
        {
            _value = value;
        }

}; // class SimMotor
