/*
   Simulate sensors using vehicle dynamics

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/Dynamics.hpp"

#include <state.hpp>

#include <RFT_sensor.hpp>
#include <RFT_filters.hpp>

class SimSensors : public rft::Sensor {

    protected:

        // We do all dynamcics => state conversion; subclasses just return sensor values
        Dynamics * _dynamics = NULL;

        virtual void modifyState(rft::State * state, float time) override
        {
            (void)time;

            hf::State * hfstate = (hf::State *)state;

            // Use vehicle state to modify Hackflight state values
            for (uint8_t k=0; k<Dynamics::STATE_SIZE; ++k) {
                hfstate->x[k] = _dynamics->x(k);
            }

            // Negate for NED => ENU conversion
            hfstate->x[hf::State::Z] *= -1;
            hfstate->x[hf::State::DZ] *= -1;

            // Rotate inertial velocity into body frame for simulating optical flow
            //rft::Filter::inertial2body(state.inertialVel, dynamicsState.pose.rotation, state.bodyVel);
        }

    public:

        SimSensors(Dynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimSensor
