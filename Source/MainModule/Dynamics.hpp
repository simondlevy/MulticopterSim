/*
 * Header-only code for platform-independent flight dynamics
 *
 * Should work for any simulator, vehicle, or operating system
 *
 * Based on:
 *
 *   @inproceedings{DBLP:conf/icra/BouabdallahMS04,
 *     author    = {Samir Bouabdallah and Pierpaolo Murrieri and Roland Siegwart},
 *     title     = {Design and Control of an Indoor Micro Quadrotor},
 *     booktitle = {Proceedings of the 2004 {IEEE} International Conference on Robotics and
 *                 Automation, {ICRA} 2004, April 26 - May 1, 2004, New Orleans, LA, {USA}},
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
#include <string.h>
#include <math.h>

#include "../Utils.hpp"
#include "../Transforms.hpp"

class Dynamics {

    public:

        /**
         * Position map for state vector
         */
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
            STATE_PSI_DOT,
            STATE_SIZE
        };


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
            double accelNED[3] = {};
            Transforms::bodyZToInertial(-_U1 / _m, euler, accelNED);

            // We're airborne once net downward acceleration goes below zero
            double netz = accelNED[2] + g;

            double velz = _x[STATE_Z_DOT];

            //debugline("Airborne: %d   AGL: %3.2f   velz: %+3.2f   netz: %+3.2f", _airborne, _agl, velz, netz);

            // If we're airborne, check for low AGL on descent
            if (_airborne) {

                //if (_agl <= 0 && velz > 0) {
                if (_agl <= 0 && netz >= 0) {
                    _airborne = false;
                    _x[STATE_PHI_DOT] = 0;
                    _x[STATE_THETA_DOT] = 0;
                    _x[STATE_PSI_DOT] = 0;
                    _x[STATE_X_DOT] = 0;
                    _x[STATE_Y_DOT] = 0;
                    _x[STATE_Z_DOT] = 0;

                    _x[STATE_PHI] = 0;
                    _x[STATE_THETA] = 0;
                    _x[STATE_Z] += _agl;
                }
            }

            // If we're not airborne, we become airborne when downward acceleration has become negative
                else {
                    _airborne = netz < 0;
                }

                // Once airborne, we can update dynamics
                if (_airborne) {

                    // Compute the state derivatives using Equation 12
                    computeStateDerivative(accelNED, netz);

                    // Compute state as first temporal integral of first temporal derivative
                    for (uint8_t i = 0; i < 12; ++i) {
                        _x[i] += dt * _dxdt[i];
                    }

                    // Once airborne, inertial-frame acceleration is same as NED acceleration
                    _inertialAccel[0] = accelNED[0];
                    _inertialAccel[1] = accelNED[1];
                    _inertialAccel[2] = accelNED[2];
                }
                else {
                    //"fly" to agl=0
                    double vz = 5 * _agl;
                    _x[STATE_Z] += vz * dt;
                }

                updateGimbalDynamics(dt);

            } // update

            // State-vector accessor
            double x(uint8_t k)
            {
                return _x[k];
            }

    protected:

            // Parameters
            double _b;
            double _d;
            double _m;
            double _Ix;
            double _Iy;
            double _Iz;
            double _Jr;
            double _l;
            uint16_t _maxrpm;

            Dynamics(uint8_t motorCount,
                    const double b, 
                    const double d, 
                    const double m, 
                    const double Ix, 
                    const double Iy, 
                    const double Iz, 
                    const double Jr, 
                    const double l,
                    uint16_t maxrpm)
            {
                _motorCount = motorCount;
                _rotorCount = motorCount; // can be overridden for thrust-vectoring

                _b = b;
                _d = d;
                _m = m;
                _Ix = Ix;
                _Iy = Iy;
                _Iz = Iz;
                _Jr = Jr;
                _l = l;
                _maxrpm = maxrpm;

                for (uint8_t i = 0; i < 12; ++i) {
                    _x[i] = 0;
                }

                _omegas = new double[motorCount]();
                _omegas2 = new double[motorCount]();
            }

            // Flag for whether we're airborne and can update dynamics
            bool _airborne = false;

            // Inertial-frame acceleration
            double _inertialAccel[3] = {};

            // y = Ax + b helper for frame-of-reference conversion methods
            static void dot(double A[3][3], double x[3], double y[3])
            {
                for (uint8_t j = 0; j < 3; ++j) {
                    y[j] = 0;
                    for (uint8_t k = 0; k < 3; ++k) {
                        y[j] += A[j][k] * x[k];
                    }
                }
            }

            // bodyToInertial method optimized for body X=Y=0
            static void bodyZToInertial(double bodyZ, const double rotation[3], double inertial[3])
            {
                double phi = rotation[0];
                double theta = rotation[1];
                double psi = rotation[2];

                double cph = cos(phi);
                double sph = sin(phi);
                double cth = cos(theta);
                double sth = sin(theta);
                double cps = cos(psi);
                double sps = sin(psi);

                // This is the rightmost column of the body-to-inertial rotation matrix
                double R[3] = { sph * sps + cph * cps * sth,
                    cph * sps * sth - cps * sph,
                    cph * cth };

                for (uint8_t i = 0; i < 3; ++i) {
                    inertial[i] = bodyZ * R[i];
                }
            }

            // Height above ground, set by kinematics
            double _agl = 0;

            // universal constants
            static constexpr double g = 9.80665; // might want to allow this to vary!

            // state vector (see Eqn. 11) and its first temporal derivative
            double _x[12] = {};
            double _dxdt[12] = {};

            // Values computed in Equation 6
            double _U1 = 0;     // total thrust
            double _U2 = 0;     // roll thrust right
            double _U3 = 0;     // pitch thrust forward
            double _U4 = 0;     // yaw thrust clockwise
            double _Omega = 0;  // torque clockwise

            // Torques about Euler angles.  We need motorvals for thrust vectoring.
            virtual void computeTorques(double * motorvals, double & u2, double & u3, double & u4) = 0;

            // radians per second for each motor, and their squared values
            double* _omegas = NULL;
            double* _omegas2 = NULL;

            // quad, hexa, octo, etc.
            uint8_t _rotorCount = 0;

            // For thrust vectoring, we can have four motors: two rotors and two servos.  For multirotors,
            // rotorCount = motorCount
            uint8_t _motorCount = 0;


            virtual void updateGimbalDynamics(double dt) {}

            /**
             * Implements Equation 12 computing temporal first derivative of state.
             * Should fill _dxdx[0..11] with appropriate values.
             * @param accelNED acceleration in NED inertial frame
             * @param netz accelNED[2] with gravitational constant added in
             */
            void computeStateDerivative(double accelNED[3], double netz)
            {
                double phidot = _x[STATE_PHI_DOT];
                double thedot = _x[STATE_THETA_DOT];
                double psidot = _x[STATE_PSI_DOT];

                _dxdt[0] = _x[STATE_X_DOT];                                                              // x'
                _dxdt[1] = accelNED[0];                                                                  // x''
                _dxdt[2] = _x[STATE_Y_DOT];                                                              // y'
                _dxdt[3] = accelNED[1];                                                                  // y''
                _dxdt[4] = _x[STATE_Z_DOT];                                                              // z'
                _dxdt[5] = netz;                                                                         // z''
                _dxdt[6] = phidot;                                                                       // phi'
                _dxdt[7] = psidot * thedot * (_Iy - _Iz) / _Ix - _Jr / _Ix * thedot * _Omega + _U2 / _Ix;    // phi''
                _dxdt[8] = thedot;                                                                       // theta'
                _dxdt[9] = -(psidot * phidot * (_Iz - _Ix) / _Iy + _Jr / _Iy * phidot * _Omega + _U3 / _Iy); // theta''
                _dxdt[10] = psidot;                                                                        // psi'
                _dxdt[11] = thedot * phidot * (_Ix - _Iy) / _Iz + _U4 / _Iz;                               // psi''
            }


            /**
             * Computes motor speed base on motor value
             * @param motorval motor value in [0,1]
             * @return motor speed in rad/s
             */
            virtual double computeMotorSpeed(double motorval)
            {
                return motorval * _maxrpm * 3.14159 / 30;
            }

    public:

            /**
             *  Destructor
             */
            virtual ~Dynamics(void)
            {
                delete _omegas;
                delete _omegas2;
            }

            /**
             * Initializes kinematic pose, with flag for whether we're airbone (helps with testing gravity).
             *
             * @param rotation initial rotation
             * @param airborne allows us to start on the ground (default) or in the air (e.g., gravity test)
             */
            void init(double rotation[3], bool airborne = false)
            {
                // Always start at location (0,0,0)
                _x[STATE_X] = 0;
                _x[STATE_Y] = 0;
                _x[STATE_Z] = 0;

                _x[STATE_PHI] = rotation[0];
                _x[STATE_THETA] = rotation[1];
                _x[STATE_PSI] = rotation[2];

                // Initialize velocities and airborne flag
                _airborne = airborne;
                _x[STATE_X_DOT] = 0;
                _x[STATE_Y_DOT] = 0;
                _x[STATE_Z_DOT] = 0;
                _x[STATE_PHI_DOT] = 0;
                _x[STATE_THETA_DOT] = 0;
                _x[STATE_PSI_DOT] = 0;

                // Initialize inertial frame acceleration in NED coordinates
                bodyZToInertial(-g, rotation, _inertialAccel);

                // We usuall start on ground, but can start in air for testing
                _airborne = airborne;
            }

            /**
             * Uses motor values to implement Equation 6.
             *
             * @param motorvals in interval [0,1] (rotors) or [-0.5,+0.5] (servos)
             */
            void setMotors(double* motorvals)
            {
                // Convert the  motor values to radians per second
                for (unsigned int i = 0; i < _rotorCount; ++i) {
                    _omegas[i] = computeMotorSpeed(motorvals[i]); //rad/s
                }

                // Overall thrust U1 is sum of squared omegas
                _U1 = 0;
                for (unsigned int i = 0; i < _rotorCount; ++i) {
                    _omegas2[i] = _omegas[i] * _omegas[i];
                    _U1 += _b * _omegas2[i];
                }

                // Torque forces are computed differently for each vehicle configuration
                double u2=0, u3=0, u4=0;
                computeTorques(motorvals, u2, u3, u4);

                _U2 = _l * _b * u2;
                _U3 = _l * _b * u3;
                _U4 = _b * u4;
            }


            /**
             * Sets height above ground level (AGL).
             * This method can be called by the kinematic visualization.
             */
            void setAgl(double agl)
            {
                _agl = agl;
            }

            // Rotor direction for animation
            virtual int8_t rotorDirection(uint8_t i) { (void)i; return 0; }

            /**
             * Gets rotor count set by constructor.
             * @return rotor count
             */
            uint8_t rotorCount(void)
            {
                return _rotorCount;
            }

}; // class Dynamics
