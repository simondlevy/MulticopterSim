/*
 * Class declaration for platform-independent multirotor dynamics
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

class MultirotorDynamics {

    private:

        double motorsToAngularVelocity(double motorvals[4], uint8_t a, uint8_t b, uint8_t c, uint8_t d);

        // State
        double _acceleration[3];
        double _velocity[3];
        double _position[3];
        double _rotation[3];

        bool _airborne;

    public:

        void init(double position[3], double rotation[3], bool airborne=false);

        void update(
                double dt, 
                double motorvals[4],                                   
                double imuAngularVelocityRPY[3], 
                double eulerAngles[3], 
                double velocityXYZ[3],
                double positionXYZ[3]);

        static void eulerToQuaternion(double eulerAngles[3], double quaternion[4]);

}; // class MultirotorDynamics








