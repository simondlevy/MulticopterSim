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


        virtual double getThrustCoefficient(double * motorvals) override
        {
            // Thrust coefficient is constant for fixed-pitch rotors

            (void)motorvals;
            
            return _fparams.b;
        }

        virtual void computeRollAndPitch(double * motorvals, double * omegas2, double & roll, double & pitch) override
        {
            // We've already used motorvals to compute omegas2
            (void)motorvals;

            roll = _fparams.l * _fparams.b * computeRoll(omegas2);
            pitch =  _fparams.l * _fparams.b * computePitch(omegas2);
        }

        virtual double computeRoll(double * omegas2) = 0;
        virtual double computePitch(double * omegas2) = 0;

}; // class FixedPitchDynamics
