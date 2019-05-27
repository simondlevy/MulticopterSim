/*
   Simulate sensors using vehicle dynamics

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "dynamics/MultirotorDynamics.hpp"

#include <sensor.hpp>
#include <datatypes.hpp>
#include <debugger.hpp>

class SimSensors : public hf::Sensor {

    protected:

    // We do all dynamcics => state conversion; subclasses just return sensor values
    MultirotorDynamics * _dynamics;

    virtual bool ready(float time) override
    {
        (void) time;
        return true;
    }

    virtual void modifyState(state_t & vehicleState, float time)
    {
        (void)time;

        // Get vehicle state from dynamics
        MultirotorDynamics::state_t dynamicsState = {0};
        _dynamics->getState(dynamicsState);

        // Use vehicle state to modify Hackflight state values
        vehicleState.location[2]    = -dynamicsState.pose.location[2]; // Negate for NED => ENU conversion
        vehicleState.inertialVel[2] = -dynamicsState.inertialVel[2];
        vehicleState.bodyVel[0] = dynamicsState.bodyVel[0];
        vehicleState.bodyVel[1] = dynamicsState.bodyVel[1];
    }

    public:

    SimSensors(MultirotorDynamics * dynamics)
    {
        _dynamics = dynamics;
    }

}; // class SimSensor
