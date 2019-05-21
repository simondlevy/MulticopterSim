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

        double _x[12] = {0}; 


        // Flag for whether we're airborne
        bool _airborne = false;

    protected:

        /** 
         * You must implement this method in a subclass for each vehicle.
         */
        virtual void getForces(double & Fz, double & L, double & M, double & N) = 0;

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

    public:

        /** 
         * Initializes pose, with flag for whether we're airbone (helps with testing gravity).
         */
        void init(double position[3], double rotation[3], bool airborne=false)
        {
            // Zero-out state
            
            for (uint8_t j=0; j<12; ++j) {
                _x[j] = 0;
            }

            // Set pose
           
            for (uint8_t j=0; j<3; ++j) {

                _x[j+6]     = rotation[j];
                _x[j+9]     = position[j];
            }

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

            // Use frame subclass (e.g., Iris) to convert motor values to forces 
            
            getForces(Fz, L, M, N);

            // XXX For now, we go directly from rotational force to angular velocity
            _x[3] = L; 
            _x[4] = M;
            _x[5] = N;

            // Integrate rotational velocity to get Euler angles
            for (uint8_t j=0; j<3; ++j) {
                _x[j+6] += dt * _x[j+3];
            }

            // Rename Euler angles for readability
            double phi   = _x[6];
            double theta = _x[7];
            double psi   = _x[8];

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
                    _x[j] += dt * accelXYZ[j];

                    // Integrate velocity to get position
                    _x[j+9] += dt * _x[j];
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
                angularVelocity[j] = _x[j+3];
                eulerAngles[j]     = _x[j+6];
                velocityXYZ[j]     = _x[j];
                positionXYZ[j]     = _x[j+9];
            }
        }

        // Factory method
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics








