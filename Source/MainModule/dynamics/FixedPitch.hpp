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
#include "../Axes.hpp"

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

        axes_t _mixer[10];

    protected:

        FixedPitchDynamics(
                uint8_t nmotors,
                Dynamics::vehicle_params_t &vparams,
                fixed_pitch_params_t &fparams,
                axes_t mixer [])
            : Dynamics(nmotors, vparams)
        {
            memcpy(&_fparams, &fparams, sizeof(fixed_pitch_params_t));
            memcpy(_mixer, mixer, nmotors*sizeof(axes_t));
        }


        virtual double getThrustCoefficient(double * actuators) override
        {
            // Thrust coefficient is constant for fixed-pitch rotors

            (void)actuators;
            
            return _fparams.b;
        }

        virtual void computeRollAndPitch(
                double * actuators,
                double * omegas2,
                double & roll,
                double & pitch) override
        {
            // We've already used actuators to compute omegas2
            (void)actuators;

            roll = 0;
            pitch = 0;

            for (uint8_t i=0; i<_rotorCount; ++i) {
                roll += _fparams.l * _fparams.b * omegas2[i] * getRotorRollContribution(i);
                pitch += _fparams.l * _fparams.b * omegas2[i] * getRotorPitchContribution(i);
            }
        }

        virtual int8_t getRotorDirection(uint8_t i) override
        {
            static const int8_t d[4] = {_mixer[0].z, _mixer[1].z, _mixer[2].z, _mixer[3].z};
            return d[i];
        }

        int8_t getRotorRollContribution(uint8_t i)
        {
            static const int8_t r[4] = {_mixer[0].x, _mixer[1].x, _mixer[2].x, _mixer[3].x};
            return r[i];
        }

        int8_t getRotorPitchContribution(uint8_t i)
        {
            static const int8_t p[4] = {_mixer[0].y, _mixer[1].y, _mixer[2].y, _mixer[3].y};
            return p[i];
        }

 
}; // class FixedPitchDynamics
