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

        virtual void computeForces(float deltaSeconds, TArray<float> motorValues, FVector & euler, 
                FVector & rotationForce, FVector & translationForce) override
        {
            // Convert motor values to rotational forces
            rotationForce.X = motorsToAngularForce(motorValues, 2, 3, 0, 1);
            rotationForce.Y = motorsToAngularForce(motorValues, 1, 3, 0, 2); 
            rotationForce.Z = motorsToAngularForce(motorValues, 1, 2, 0, 3); 

            // Rotate Euler angles into inertial frame: http://www.chrobotics.com/library/understanding-euler-angles
            float x = sin(euler.X)*sin(euler.Z) + cos(euler.X)*cos(euler.Z)*sin(euler.Y);
            float y = cos(euler.X)*sin(euler.Y)*sin(euler.Z) - cos(euler.Z)*sin(euler.X);
            float z = cos(euler.Y)*cos(euler.X);

            // Use rotated Euler angles to compute translation force
            translationForce = THRUST_FACTOR * sum(motorValues) * FVector(-x, -y, z);
        }

    private:

        static constexpr float THRUST_FACTOR = 130.f;

        static float motorsToAngularForce(TArray<float> motorValues, uint8_t a, uint8_t b, uint8_t c, uint8_t d)
        {
            float v = ((motorValues[a] + motorValues[b]) - (motorValues[c] + motorValues[d]));

            return (v<0 ? -1 : +1) * fabs(v);
        }

        static float sum(TArray<float> x) 
        {
            float s = 0.f;

            for (auto it = x.CreateConstIterator(); it; ++it) {
                s += *it;
            }

            return s;
        }

}; // SimpleVehiclePhysics

// Factory method
VehiclePhysics * VehiclePhysics::createVehiclePhysics(void)
{
    return new SimpleVehiclePhysics();
}



