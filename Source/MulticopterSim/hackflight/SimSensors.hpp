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

    private:

        static void rotateVelocity(state_t & vehicleState, double eulerAngles[3], double velocityXYZ[3])
        {
            // Rotate vehicle's inertial velocity into body frame
            float psi = eulerAngles[2];
            float cp = cos(psi);
            float sp = sin(psi);
            float vx = velocityXYZ[0];
            float vy = velocityXYZ[1];
            vehicleState.inertialVel[0] = vx * cp + vy * sp;
            vehicleState.inertialVel[1] = vy * cp - vx * sp;
        }

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
            rotateVelocity(vehicleState, dynamicsState.pose.rotation, dynamicsState.inertialVel);
        }

    public:

        SimSensors(MultirotorDynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimSensor
