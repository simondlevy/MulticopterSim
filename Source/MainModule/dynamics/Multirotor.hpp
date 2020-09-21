/*
* Dynamics class for multirotors
*
* Copyright (C) 2020 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../Dynamics.hpp"

class MultirotorDynamics : public Dynamics {

    protected:

        /**
         * Implements Equation 12 computing temporal first derivative of state.
         * Should fill _dxdx[0..11] with appropriate values.
         * @param accelNED acceleration in NED inertial frame
         * @param netz accelNED[2] with gravitational constant added in
         * @param phidot rotational acceleration in roll axis
         * @param thedot rotational acceleration in pitch axis
         * @param psidot rotational acceleration in yaw axis
         */
        virtual void computeStateDerivative(double accelNED[3], double netz) override
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
            _dxdt[7] = psidot * thedot * (_p->Iy - _p->Iz) / _p->Ix - _p->Jr / _p->Ix * thedot * _Omega + _U2 / _p->Ix;    // phi''
            _dxdt[8] = thedot;                                                                       // theta'
            _dxdt[9] = -(psidot * phidot * (_p->Iz - _p->Ix) / _p->Iy + _p->Jr / _p->Iy * phidot * _Omega + _U3 / _p->Iy); // theta''
            _dxdt[10] = psidot;                                                                        // psi'
            _dxdt[11] = thedot * phidot * (_p->Ix - _p->Iy) / _p->Iz + _U4 / _p->Iz;                               // psi''
        }

    public:

        MultirotorDynamics(Parameters * params, const uint8_t motorCount) 
            : Dynamics(motorCount)
        {
            _p = params;

            _omegas = new double[motorCount]();
            _omegas2 = new double[motorCount]();
        }

        /**
         * Updates state.
         *
         * @param dt time in seconds since previous update
         */
        virtual void update(double dt) override
        {
            // Use the current Euler angles to rotate the orthogonal thrust vector into the inertial frame.
            // Negate to use NED.
            double euler[3] = { _x[6], _x[8], _x[10] };
            double accelNED[3] = {};
            bodyZToInertial(-_U1 / _p->m, euler, accelNED);

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

                // Get most values directly from state vector
                for (uint8_t i = 0; i < 3; ++i) {
                    uint8_t ii = 2 * i;
                    _state.angularVel[i] = _x[STATE_PHI_DOT + ii];
                    _state.inertialVel[i] = _x[STATE_X_DOT + ii];
                    _state.pose.rotation[i] = _x[STATE_PHI + ii];
                    _state.pose.location[i] = _x[STATE_X + ii];
                }

                // Convert inertial acceleration and velocity to body frame
                inertialToBody(_inertialAccel, _state.pose.rotation, _state.bodyAccel);

                // Convert Euler angles to quaternion
                eulerToQuaternion(_state.pose.rotation, _state.quaternion);

            } // update

            /**
             * Uses motor values to implement Equation 6.
             *
             * @param motorvals in interval [0,1]
             * @param dt time constant in seconds
             */
            virtual void setMotors(double* motorvals, double dt) override
            {
                // Convert the  motor values to radians per second
                for (unsigned int i = 0; i < _motorCount; ++i) {
                    _omegas[i] = computeMotorSpeed(motorvals[i]); //rad/s
                }

                // Compute overall torque from omegas before squaring
                _Omega = u4(_omegas);

                // Overall thrust is sum of squared omegas
                _U1 = 0;
                for (unsigned int i = 0; i < _motorCount; ++i) {
                    _omegas2[i] = _omegas[i] * _omegas[i];
                    _U1 += _p->b * _omegas2[i];
                }

                // Use the squared Omegas to implement the rest of Eqn. 6
                _U2 = _p->l * _p->b * u2(_omegas2);
                _U3 = _p->l * _p->b * u3(_omegas2);
                _U4 = _p->d * u4(_omegas2);
            }

}; // class MultirotorDynamics
