/*
 * Class declaration for platform-independent multirotor dynamics
 *
 * Based on equations in 
 *
 *   https://charlestytler.com/modeling-vehicle-dynamics-6dof-nonlinear-simulation/
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

        // Earth's gravitational constant.  Eventually we may want to make this a variable.
        static constexpr double G = 9.80665;
        
        // State vector
        // x[0]  u:   longitudinal velocity
        // x[1]  v:   lateral velocity
        // x[2]  w:   normal velocity
        // x[3]  p:   roll rate
        // x[4]  q:   pitch rate
        // x[5]  r:   yaw rate
        // x[6]  phi: bank angle
        // x[7]  the: pitch attitude
        // x[8]  psi: heading
        // x[9]  xE:  longitudinal position in Earth frame
        // x[10] yE:  lateral position in Earth frame
        // x[11] hE:  height in Earth frame
        double _x[12]; 

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








