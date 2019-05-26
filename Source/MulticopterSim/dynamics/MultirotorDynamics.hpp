/*
 * Header-only code for platform-independent multirotor dynamics
 *
 * Should work for any simulator, vehicle, or operating system
 *
 * Based on:
 *
 *   @inproceedings{DBLP:conf/icra/BouabdallahMS04,
 *     author    = {Samir Bouabdallah and Pierpaolo Murrieri and Roland Siegwart},
 *     title     = {Design and Control of an Indoor Micro Quadrotor},
 *     booktitle = {Proceedings of the 2004 {IEEE} International Conference on Robotics and 
                    Automation, {ICRA} 2004, April 26 - May 1, 2004, New Orleans, LA, {USA}},
 *     pages     = {4393--4398},
 *     year      = {2004},
 *     crossref  = {DBLP:conf/icra/2004},
 *     url       = {https://doi.org/10.1109/ROBOT.2004.1302409},
 *     doi       = {10.1109/ROBOT.2004.1302409},
 *     timestamp = {Sun, 04 Jun 2017 01:00:00 +0200},
 *     biburl    = {https://dblp.org/rec/bib/conf/icra/BouabdallahMS04},
 *     bibsource = {dblp computer science bibliography, https://dblp.org}
 *   }
 * 
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <math.h>

class MultirotorDynamics {

    private:

        // Universal constants
        static constexpr double g  = 9.80665; // might want to allow this to vary!
        static constexpr double pi = 3.14159;

        // State vector (see Eqn. 11)
        double _x[12] = {0};

        // State vector position map
        enum {
            STATE_X, 
            STATE_X_DOT, 
            STATE_Y,
            STATE_Y_DOT,
            STATE_Z,
            STATE_Z_DOT,
            STATE_PHI,
            STATE_PHI_DOT,
            STATE_THETA,
            STATE_THETA_DOT,
            STATE_PSI,
            STATE_PSI_DOT
        };

        // Set by subclass constructor
        unsigned int _nmotors = 0;

        // Values computed in Equation 6
        double _U1 = 0;     // total thrust
        double _U2 = 0;     // roll thrust right
        double _U3 = 0;     // pitch thrust forward
        double _U4 = 0;     // yaw thrust clockwise
        double _Omega = 0;  // torque clockwise

        // Radians per second for each motor
        double * _omegas = NULL;

        // Body-frame acceleration
        double _inertialAccel[3] = {0};

        // Flag for whether we're airborne
        bool _airborne = false;

        // Support for debugging
        char _message[200];
        FILE * _logfp = NULL;
        double _logtime = 0;
        double _motormean = 0;

        // y = Ax + b helper for frame-of-reference conversion methods
        static void dot(double A[3][3], double x[3], double y[3])
        {
            for (uint8_t j=0; j<3; ++j) {
                y[j] = 0;
                for (uint8_t k=0; k<3; ++k) {
                    y[j] += A[j][k] * x[k];
                }
            }
        }

        // bodyToInertial() method optimized for body X=Y=0
        static void bodyZToInertial(double bodyZ, double rotation[3], double inertial[3])
        {
            double phi   = rotation[0];
            double theta = rotation[1];
            double psi   = rotation[2];

            double cph = cos(phi);
            double sph = sin(phi);
            double cth = cos(theta);
            double sth = sin(theta);
            double cps = cos(psi);
            double sps = sin(psi);

            // This is the rightmost column of the body-to-inertial rotation matrix
            double R[3] = {sph*sps + cph*cps*sth, 
                           cph*sps*sth - cps*sph, 
                           cph*cth};

            for (uint8_t i=0; i<3; ++i) {
                inertial[i] = bodyZ * R[i];
            }
        }

    protected:

        /** 
         * You must implement these constant methods in a subclass for each vehicle.
         */
        virtual const double b(void)  = 0;
        virtual const double d(void)  = 0;
        virtual const double m(void)  = 0;
        virtual const double l(void)  = 0;
        virtual const double Ix(void) = 0;
        virtual const double Iy(void) = 0;
        virtual const double Iz(void) = 0; 
        virtual const double Jr(void) = 0;
        virtual const unsigned int maxrpm(void) = 0;

        /** 
         * You must implement these methods in a subclass for each vehicle.
         */
        virtual double u2(double * o2)  = 0;
        virtual double u3(double * o2)  = 0;
        virtual double u4(double * o2)  = 0;
        virtual double omega(double * o) = 0;

        /**
         *  Constructor
         */
        MultirotorDynamics(unsigned int nmotors)
        {
            _omegas = new double[nmotors];
            _nmotors = nmotors;
            
            // Zero-out entire state
            for (int i=0; i<12; ++i) {
                _x[i] = 0;
            }
        }

    public:

        /**
         *  Destructor
         */
        virtual ~MultirotorDynamics(void)
        {
            delete _omegas;
            //fclose(_logfp);
        }

        /** 
         * Initializes kinematic pose, with flag for whether we're airbone (helps with testing gravity).
         *
         * @param location X,Y,Z
         * @param rotation phi, theta, psi
         * @param airborne allows us to start on the ground (default) or in the air (e.g., gravity test)
         */
        void init(double location[3], double rotation[3], bool airborne=false)
        {
            // Initialize pose
            for (int i=0; i<3; ++i) {
                _x[STATE_X+i]   = location[i];
                _x[STATE_PHI+i] = rotation[i];
            }

            // Initialize inertial frame acceleration in NED coordinates
            bodyZToInertial(-g, rotation, _inertialAccel);

            // We can start on the ground (default) or in the air
            _airborne = airborne;

            //_logfp = fopen("C:\\Users\\sim\\Desktop\\log.csv", "w");
            _logtime = 0;
            
        }

        /** 
         * Updates state.
         *
         * @param dt time in seconds since previous update
         */
        void update(double dt)
        {
            // Use the current Euler angles to rotate the orthogonal thrust vector into the inertial frame.
            // Negate to use NED.
            double euler[3] = { _x[6], _x[8], _x[10] };
            double down[3]  = {0};
            bodyZToInertial(-_U1/m(), euler, down);

            // We're airborne once net downward acceleration goes below zero
            double netz = down[2] + g;
            if (!_airborne) {
                _airborne = netz < 0;
            }

            // Once airborne, we can update dynamics
            if (_airborne) {

                // Make some useful abbreviations
                double phidot = _x[STATE_PHI_DOT];
                double thedot = _x[STATE_THETA_DOT];
                double psidot = _x[STATE_PSI_DOT];

                double dxdt[12] = {

                    // Equation 12: compute temporal first derivative of state.
                    /* x'      */ _x[STATE_X_DOT],
                    /* x''     */ down[0],
                    /* y'      */ _x[STATE_Y_DOT],
                    /* y''     */ down[1],
                    /* z'      */ _x[STATE_Z_DOT],
                    /* z''     */ netz,
                    /* phi'    */ phidot,
                    /* phi''   */ psidot*thedot*(Iy()-Iz())/Ix()   - Jr()/Ix()*thedot*_Omega + l()/Ix()*_U2,
                    /* theta'  */ thedot,
                    /* theta'' */ -(psidot*phidot*(Iz()-Ix())/Iy() + Jr()/Iy()*phidot*_Omega + l()/Iy()*_U3), 
                    /* psi'    */ psidot,
                    /* psi''   */ thedot*phidot*(Ix()-Iy())/Iz()   + l()/Iz()*_U4,
                };

                // Compute state as first temporal integral of first temporal derivative
                for (int i=0; i<12; ++i) {
                    _x[i] += dt * dxdt[i];
                }

                // Once airborne, inertial-frame acceleration is same as downward acceleration
                _inertialAccel[0] = down[0];
                _inertialAccel[1] = down[1];
                _inertialAccel[2] = down[2];
            }

            sprintf_s(_message, "M: %3.3f  |  AX: %+3.3f    AY: %+3.3f    AZ: %+3.3f", 
                    _motormean, _inertialAccel[0], _inertialAccel[1], _inertialAccel[2]);

            //fprintf(_logfp, "%f,%+3.3f,%+3.3f,%+3.3f\n", _logtime, g-_inertialAccel[2], _x[STATE_Z_DOT], _x[STATE_Z]);

            _logtime += dt;
        }

        /**
         * Uses motor values to implement Equation 6.
         *
         * @param motorvals in interval [0,1]
         */
        void setMotors(double * motorvals) 
        {
            // Debugging
            _motormean = 0;

            // Convert the  motor values to radians per second
            for (unsigned int i=0; i<_nmotors; ++i) {
                _omegas[i] = motorvals[i] * maxrpm() * pi / 30;
                _motormean += motorvals[i];
            }

            _motormean /= _nmotors;

            // Compute overall torque from Omegas
            _Omega = omega(_omegas);

            // Overall thrust is sum of squared omegas
            _U1 = 0;
            for (unsigned int i=0; i<_nmotors; ++i) {
                _omegas[i] *= _omegas[i];
                _U1 += b() * _omegas[i];
            }

            // Use the squared Omegas to implement the rest of Eqn. 6
            _U2 = b() * u2(_omegas);
            _U3 = b() * u3(_omegas);
            _U4 = d() * u4(_omegas);

        }

        /*
         *  Gets current state
         *
         *  @param angularVel  angular velocity radians / second, in body frame
         *  @param bodyAccel   linear acceleration in body frame
         *  @param rotation    Euler angles in radians
         *  @param inertialVel linear velocity in inertial frame
         *  @param location    location in inertial frame
         *
         *  @return true if crashed, false otherwise
         */
        bool getState(double angularVel[3], double bodyAccel[3], double rotation[3], double inertialVel[3], double location[3])
        {
            // Get most values directly from state
            for (int i=0; i<3; ++i) {
                angularVel[i]  = _x[STATE_PHI_DOT+2*i];
                rotation[i]    = _x[STATE_PHI+2*i];
                location[i]    = _x[STATE_X+2*i];
                inertialVel[i] = _x[STATE_X_DOT+2*i];
            }

            // Convert inertial acceleration to body frame
            inertialToBody(_inertialAccel, rotation, bodyAccel);

            // If we're airborne, we've crashed if we fall below ground level
            return _airborne ? (location[2] > 0) : false;
        }

        /**
         *  Supports debugging
         *
         * @return message string that can be displayed by calling program (e.g., FlightManager)
         */
        char * getMessage(void)
        {
            return _message;
        }

        /**
         *  Frame-of-reference conversion routines.
         *
         *  See Section 5 of http://www.chrobotics.com/library/understanding-euler-angles
         */

        static void bodyToInertial(double body[3], double rotation[3], double inertial[3])
        {
            double phi   = rotation[0];
            double theta = rotation[1];
            double psi   = rotation[2];

            double cph = cos(phi);
            double sph = sin(phi);
            double cth = cos(theta);
            double sth = sin(theta);
            double cps = cos(psi);
            double sps = sin(psi);

            double R[3][3] = { {cps*cth,  cps*sph*sth - cph*sps,  sph*sps + cph*cps*sth}, 
                               {cth*sps,  cph*cps + sph*sps*sth,  cph*sps*sth - cps*sph}, 
                               {-sth,     cth*sph,                cph*cth}               };

            dot(R, body, inertial);
        }

        static void inertialToBody(double inertial[3], double rotation[3], double body[3])
        {
            double phi   = rotation[0];
            double theta = rotation[1];
            double psi   = rotation[2];

            double cph = cos(phi);
            double sph = sin(phi);
            double cth = cos(theta);
            double sth = sin(theta);
            double cps = cos(psi);
            double sps = sin(psi);

            double R[3][3] = { {cps*cth,                cth*sps,                   -sth}, 
                               {cps*sph*sth - cph*sps,  cph*cps + sph*sps*sth,  cth*sph}, 
                               {sph*sps + cph*cps*sth,  cph*sps*sth - cps*sph,  cph*cth} };

            dot(R, inertial, body);
        }


        /**
         * Factory method
         */
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics
