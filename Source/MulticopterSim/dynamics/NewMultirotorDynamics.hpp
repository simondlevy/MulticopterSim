/*
 * Header-only code for platform-independent multirotor dynamics
 *
 * Should work for any simulator, vehicle, or operating system
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include <math.h>

class MultirotorDynamics {

    private:

        // Earth's gravitational constant.  Eventually we may want to make this a variable.
        static constexpr double G = 9.80665;
        
        // M_PI is not defined on some systems
#ifndef M_PI
        static constexpr double M_PI = 3.14159265358979323846;
#endif

        double _x   = 0;
        double _xd  = 0;
        double _xdd = 0;
        double _y   = 0;
        double _yd  = 0;
        double _ydd = 0;
        double _z   = 0;
        double _zd  = 0;
        double _zdd = 0;
        double _phi   = 0;
        double _phid  = 0;
        double _phidd = 0;
        double _theta   = 0;
        double _thetad  = 0;
        double _thetadd = 0;
        double _psi   = 0;
        double _psid  = 0;
        double _psidd = 0;


        // Flag for whether we're airborne
        bool _airborne = false;

    protected:

        /** 
         * You must implement this method in a subclass for each vehicle.
         */
        virtual void getForces(double & U1, double & U2, double & U3, double & U4, double & Omega) = 0;

        /**
         *  Converts motor value in [0,1] to radians per second
         */
        static double rps(double motorval, const double maxrpm)
        {
            return motorval * maxrpm * M_PI / 30; 
        }

    public:

        /** 
         * Initializes pose, with flag for whether we're airbone (helps with testing gravity).
         */
        void init(double position[3], double rotation[3], bool airborne=false)
        {
            // Zero-out state first derivatives
            
            _xd  = 0;
            _xdd = 0;
            _yd  = 0;
            _ydd = 0;
            _zd  = 0;
            _zdd = 0;
            _phid  = 0;
            _phidd = 0;
            _thetad  = 0;
            _thetadd = 0;
            _psid  = 0;
            _psidd = 0;

            // Set pose
           
            _x = position[0];
            _y = position[1];
            _z = position[2];

            _phi   = rotation[0];
            _theta = rotation[1];
            _psi   = rotation[2];

            _airborne = airborne;
        }

        /** 
         * Updates dynamics state.
         */

        void update(double dt)
        {
            // Forces
            double U1 = 0;
            double U2 = 0;
            double U3 = 0;
            double U4 = 0;
            double Omega = 0;

            // Use frame subclass (e.g., Iris) to convert motor values to forces 
            getForces(U1, U2, U3, U4, Omega);
        }

        /**
         * Sets motor values.
         * You must implement this method in a subclass for each vehicle.
         */
        virtual void setMotors(double * motorvals) = 0;

        /*
         *  Gets current state
         */

        void getState(double angularVelocity[3], double eulerAngles[3], double velocityXYZ[3], double positionXYZ[3])
        {
        }

        // Factory method
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics








