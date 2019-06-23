/*
   Simulate sensors using vehicle dynamics

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MulticopterSim/dynamics/MultirotorDynamics.hpp"

#include <sensor.hpp>
#include <datatypes.hpp>
#include <debugger.hpp>

class SimSensors : public hf::Sensor {

    private:

        // Helper
        static void inertialToBody(float inertial[3], double rotation[3], float body[3])
        {
            double di[3] = {inertial[0], inertial[1], inertial[2]};
            double bi[3] = {0};
            MultirotorDynamics::inertialToBody(di, rotation, bi);
            body[0] = bi[0];
            body[1] = bi[1];
            body[2] = bi[2];
        }

    protected:

        // We do all dynamcics => state conversion; subclasses just return sensor values
        MultirotorDynamics * _dynamics;

        virtual bool ready(float time) override
        {
            (void) time;
            return true;
        }

        virtual void modifyState(hf::state_t & vehicleState, float time)
        {
            (void)time;

            // Get vehicle state from dynamics
            MultirotorDynamics::state_t dynamicsState;
            _dynamics->getState(dynamicsState);

            // Use vehicle state to modify Hackflight state values
            vehicleState.location[2]    = -dynamicsState.pose.location[2]; // Negate for NED => ENU conversion
            vehicleState.inertialVel[2] = -dynamicsState.inertialVel[2];

            // Rotate inertial velocity into body frame for simulating optical flow
            inertialToBody(vehicleState.inertialVel, dynamicsState.pose.rotation, vehicleState.bodyVel);
        }

    public:

        SimSensors(MultirotorDynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimSensor
