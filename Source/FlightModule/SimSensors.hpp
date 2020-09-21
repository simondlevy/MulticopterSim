/*
   Simulate sensors using vehicle dynamics

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/Dynamics.hpp"

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
            Dynamics::inertialToBody(di, rotation, bi);
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

        virtual void modifyState(hf::state_t & vehicleState, float time)
        {
            (void)time;

            // Get vehicle state from dynamics
            Dynamics::state_t dynamicsState = _dynamics->getState();

            // Use vehicle state to modify Hackflight state values
            for (uint8_t k=0; k<3; ++k) {
                vehicleState.location[k]    = dynamicsState.pose.location[k]; 
                vehicleState.inertialVel[k] = dynamicsState.inertialVel[k];
            }

            // Negate for NED => ENU conversion
            vehicleState.location[2]    *= -1;
            vehicleState.inertialVel[2] *= -1;

            // Rotate inertial velocity into body frame for simulating optical flow
            inertialToBody(vehicleState.inertialVel, dynamicsState.pose.rotation, vehicleState.bodyVel);
        }

    public:

        SimSensors(Dynamics * dynamics)
        {
            _dynamics = dynamics;
        }

}; // class SimSensor
