/*
   Simulate sensors using vehicle dynamics

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/Dynamics.hpp"
#include "../MainModule/Transforms.hpp"

#include <state.hpp>
#include <sensor.hpp>

class SimSensors : public hf::Sensor {

    private:

        // Helper
        static void inertialToBody(float inertial[3], double rotation[3], float body[3])
        {
            double di[3] = {inertial[0], inertial[1], inertial[2]};
            double bi[3] = {0};
            Transforms::inertialToBody(di, rotation, bi);
            body[0] = bi[0];
            body[1] = bi[1];
            body[2] = bi[2];
        }

    protected:

        // We do all dynamcics => state conversion; subclasses just return sensor values
        Dynamics * _dynamics;

        virtual bool ready(float time) override
        {
            (void) time;
            return true;
        }

        virtual void modifyState(hf::State * state, float time)
        {
            (void)time;

            // Use vehicle state to modify Hackflight state values
            for (uint8_t k=0; k<Dynamics::STATE_SIZE; ++k) {
                state->x[k] = _dynamics->x(k);
            }

            // Negate for NED => ENU conversion
            state->x[hf::State::STATE_Z] *= -1;
            state->x[hf::State::STATE_DZ] *= -1;

            // Rotate inertial velocity into body frame for simulating optical flow
            //inertialToBody(state.inertialVel, dynamicsState.pose.rotation, state.bodyVel);
        }

    public:

        SimSensors(Dynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimSensor
