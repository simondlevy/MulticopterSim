/*
 * Header-only code for dynamics of vehicles with fixed rotor pitch
 * (quadcopter, hexacopter, ocotocopter, ...)
 *
 * Copyright (C) 2021 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Dynamics.hpp"


class FixedPitchDynamics : public Dynamics {

    public:

        /**
         *  Vehicle parameters
         */
        typedef struct {

            double b;  // thrust coefficient [F=b*w^2]
            double l;  // arm length [m]

        } fixed_pitch_params_t; 

    private:

        fixed_pitch_params_t _fparams;

    protected:

        FixedPitchDynamics(uint8_t nmotors, Dynamics::vehicle_params_t &vparams, fixed_pitch_params_t &fparams)
            : Dynamics(nmotors, vparams)
        {
            memcpy(&_fparams, &fparams, sizeof(fixed_pitch_params_t));
        }

        virtual void computeForces(double* motorvals) override
        {
            // Overall thrust U1 is sum of squared omegas
            _U1 = 0;
            for (unsigned int i = 0; i < _rotorCount; ++i) {
                _omegas2[i] = _wparams.rho * _omegas[i] * _omegas[i];
                _U1 += _fparams.b * _omegas2[i];
            }

            _U2 = _fparams.l * _fparams.b * u2();
            _U3 = _fparams.l * _fparams.b * u3();
            _U4 = _fparams.b * u4();
            _Omega = omega();
        }

        virtual double u2(void) = 0;
        virtual double u3(void) = 0;
        virtual double u4(void) = 0;
        virtual double omega(void) = 0;

}; // class FixedPitchDynamics
