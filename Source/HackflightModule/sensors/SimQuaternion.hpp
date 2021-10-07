/*
   Simulate quaternion "sensor" using vehicle kinematics

   We go directly from the Euler angles in the kinematics to the Euler angles in the
   vehicle state.

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/Dynamics.hpp"

#include <HF_state.hpp>

#include <HF_sensor.hpp>
#include <HF_filters.hpp>

class SimQuaternion : public hf::Sensor {

    protected:

        // We do all dynamcics => state conversion; subclasses just return
        // sensor values
        Dynamics * _dynamics = NULL;

        virtual void modifyState(hf::State * state, float time) override
        {
            (void)time;

            hf::State * hfstate = (hf::State *)state;

            hfstate->x[hf::State::PHI] = _dynamics->x(hf::State::PHI);
            hfstate->x[hf::State::THETA] = _dynamics->x(hf::State::THETA);
            hfstate->x[hf::State::PSI] = _dynamics->x(hf::State::PSI);
        }

    public:

        SimQuaternion(Dynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimQuaternion
