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

        virtual void computeAngularForces(TArray<float> motorvals, FVector & forces, float & overallThrust) override
        {
            // Sum over motor values to get overall thrust
            float motorSum = 0;
            for (uint8_t k=0; k<4; ++k) {
                motorSum += motorvals[k];
            }

            forces.X = motorsToAngularForce(motorvals, 2, 3, 0, 1);
            forces.Y = motorsToAngularForce(motorvals, 1, 3, 0, 2); 
            forces.Z = motorsToAngularForce(motorvals, 1, 2, 0, 3); 

            overallThrust = motorSum * THRUST_FACTOR;
        }

    private:

        static constexpr float THRUST_FACTOR = 130.f;

        float motorsToAngularForce(TArray<float> motorvals, int a, int b, int c, int d)
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



