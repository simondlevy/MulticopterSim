/*
 * SimpleVehiclePhysics.cpp: simple vehicle physics implementation for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "VehiclePhysics.h"

class SimpleVehiclePhysics : public VehiclePhysics {

    protected:

        virtual void computeAngularForces(float motorvals[4], float forces[3], float & overallThrust) override
        {
            // Sum over motor values to get overall thrust
            float motorSum = 0;
            for (uint8_t k=0; k<4; ++k) {
                motorSum += motorvals[k];
            }

            forces[0] = motorsToAngularForce(motorvals, 2, 3, 0, 1);
            forces[1] = motorsToAngularForce(motorvals, 1, 3, 0, 2); 
            forces[2] = motorsToAngularForce(motorvals, 1, 2, 0, 3); 

            overallThrust = motorSum * THRUST_FACTOR;
        }

    private:

        static constexpr float THRUST_FACTOR = 130.f;

        float motorsToAngularForce(float motorvals[4], int a, int b, int c, int d)
        {
            float v = ((motorvals[a] + motorvals[b]) - (motorvals[c] + motorvals[d]));

            return (v<0 ? -1 : +1) * fabs(v);
        }

}; // SimpleVehiclePhysics

// Factory method
VehiclePhysics * VehiclePhysics::createVehiclePhysics(void)
{
    return new SimpleVehiclePhysics();
}



