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

        double _xstate[12] = {0}; 

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
        virtual void getForces(double & Fz, double & L, double & M, double & N) = 0;

        virtual void getForces2(double & U1, double & U2, double & U3, double & U4) = 0;

        /*
         *  Converts motor value in [0,1] to thrust in Newtons
         *
         */
        static double Fthrust(double motorval, const double b, const double maxrpm)
        {
            double omega = motorval * maxrpm * M_PI / 30; // radians per second

            return b * (omega*omega);
        }

        /*
         * Computes thrust in Newtons to torque in Newton meters
         */

        static double T(double F, double dx, double dy)
        {
            return F; // XXX should use dx, dy
        }

        /**
         *  Converts motor value in [0,1] to square of radians per second
         */
        static double rpss(double motorval, const double maxrpm)
        {
            double omega = motorval * maxrpm * M_PI / 30; // radians per second

            return omega*omega;
        }

    public:

        /** 
         * Initializes pose, with flag for whether we're airbone (helps with testing gravity).
         */
        void init(double position[3], double rotation[3], bool airborne=false)
        {
            // Zero-out state
            
            for (uint8_t j=0; j<12; ++j) {
                _xstate[j] = 0;
            }

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
           
            for (uint8_t j=0; j<3; ++j) {

                _xstate[j+6]     = rotation[j];
                _xstate[j+9]     = position[j];
            }

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
            
            double Fz = 0;
            double L  = 0;
            double M  = 0;
            double N  = 0;

            double U1 = 0;
            double U2 = 0;
            double U3 = 0;
            double U4 = 0;

            // Use frame subclass (e.g., Iris) to convert motor values to forces 
            
            getForces(Fz, L, M, N);

            getForces2(U1, U2, U3, U4);

            // XXX For now, we go directly from rotational force to angular velocity
            _xstate[3] = L; 
            _xstate[4] = M;
            _xstate[5] = N;

            // Integrate rotational velocity to get Euler angles
            for (uint8_t j=0; j<3; ++j) {
                _xstate[j+6] += dt * _xstate[j+3];
            }

            // Rename Euler angles for readability
            double phi   = _xstate[6];
            double theta = _xstate[7];
            double psi   = _xstate[8];

            // Pre-compute angle trigonometry for rotation to earth frame
            double sphi = sin(phi);
            double spsi = sin(psi);
            double cphi = cos(phi);
            double cpsi = cos(psi);
            double sthe = sin(theta);
            double cthe = cos(theta);

            // Rotate orthongal force vector into intertial frame to compute translation force.  
            // See last column of rotation matrix at end of section 5 in
            //   http://www.chrobotics.com/library/understanding-euler-angles
            // Note use of negative sign to implement North-East-Down (NED) coordinates
            double accelXYZ[3] = { -Fz * (sphi*spsi + cphi*cpsi*sthe), -Fz * (cphi*spsi*sthe - cpsi*sphi), -Fz * (cphi*cthe) };

            // Add earth gravity to get net acceleration vertical, so that motionless maps to zero
            accelXYZ[2] += G;

            // We're airborne once net vertical acceleration falls below zero
            if (!_airborne) {
                _airborne = accelXYZ[2] < 0;
            }

            // Once airborne, we can compute inertial-frame state values by integration
            if (_airborne) {

                for (uint8_t j=0; j<3; ++j) {

                    // Integrate acceleration to get velocity
                    _xstate[j] += dt * accelXYZ[j];

                    // Integrate velocity to get position
                    _xstate[j+9] += dt * _xstate[j];
                }
            }
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
            for (uint8_t j=0; j<3; ++j) {
                angularVelocity[j] = _xstate[j+3];
                eulerAngles[j]     = _xstate[j+6];
                velocityXYZ[j]     = _xstate[j];
                positionXYZ[j]     = _xstate[j+9];
            }
        }

        // Factory method
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics








