/*
   Simulate gyrometer using vehicle kinematics

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/Dynamics.hpp"

#include <HF_state.hpp>

#include <HF_sensor.hpp>
#include <HF_filters.hpp>

class SimGyrometer : public hf::Sensor {

    protected:

        // We do all dynamcics => state conversion; subclasses just return
        // sensor values
        Dynamics * _dynamics = NULL;

        virtual void modifyState(hf::State * state, float time) override
        {
            (void)time;

            hf::State * hfstate = (hf::State *)state;

            // Negate for NED => ENU conversion
            hfstate->x[hf::State::DPHI] = _dynamics->x(hf::State::DPHI); 
            hfstate->x[hf::State::DTHETA] = _dynamics->x(hf::State::DTHETA); 
            hfstate->x[hf::State::DPSI] = _dynamics->x(hf::State::DPSI); 
        }

    public:

        SimGyrometer(Dynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimGyrometer
