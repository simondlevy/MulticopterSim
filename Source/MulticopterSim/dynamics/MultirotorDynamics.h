/*
 * Class declaration for platform-independent multirotor dynamics
 *
 * Should work for any simulator, vehicle, or operating system
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

class MultirotorDynamics {

    private:

        // Earth's gravitational constant
        static constexpr double G = 9.80665;
        
        // State
        double _angularVelocity[3];
        double _acceleration[3];
        double _velocity[3];
        double _position[3];
        double _rotation[3];

        bool _airborne;

    protected:

        /** 
         * You must implement this method in a subclass for each vehicle.
         */
        virtual void getForces(double & Fz, double & L, double & M, double & N) = 0;

    public:

        /** 
         * Initializes pose, with flag for whether we're airbone (helps with testing gravity).
         */
        void init(double position[3], double rotation[3], bool airborne=false);


        /** 
         * Updates dynamics state.
         */
        void update(double dt);


        /**
         * Sets motor values.
         * You must implement this method in a subclass for each vehicle.
         */
        virtual void setMotors(double * motorvals) = 0;

        /*
         *  Gets current state
         */
        void getState(double angularVelocity[3], double eulerAngles[3], double velocityXYZ[3], double positionXYZ[3]);

        // Not strictly part of dynamics, but useful
        static void eulerToQuaternion(double eulerAngles[3], double quaternion[4]);

        // Factory method
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics








