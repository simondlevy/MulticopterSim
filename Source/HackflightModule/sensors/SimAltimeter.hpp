/*
   Simulate altimeter using vehicle kinematics

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/Dynamics.hpp"

#include <HF_state.hpp>

#include <HF_sensor.hpp>
#include <HF_filters.hpp>

class SimAltimeter : public hf::Sensor {

    protected:

        // We do all dynamcics => state conversion; subclasses just return
        // sensor values
        Dynamics * _dynamics = NULL;

        virtual void modifyState(hf::State * state, float time) override
        {
            hf::State * hfstate = (hf::State *)state;

            // Negate for NED => ENU conversion
            hfstate->x[hf::State::Z] = -_dynamics->x(hf::State::Z);
            hfstate->x[hf::State::DZ] = -_dynamics->x(hf::State::DZ);
        }

    public:

        SimAltimeter(Dynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimAltimeter
