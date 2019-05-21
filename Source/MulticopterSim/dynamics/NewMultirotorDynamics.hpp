/*
 * Header-only code for platform-independent multirotor dynamics
 *
 * Should work for any simulator, vehicle, or operating system
 *
 * Based on:
 *
 *   @inproceedings{DBLP:conf/icra/BouabdallahMS04,
 *    author    = {Samir Bouabdallah and
 *                  Pierpaolo Murrieri and
 *                  Roland Siegwart},
 *     title     = {Design and Control of an Indoor Micro Quadrotor},
 *     booktitle = {Proceedings of the 2004 {IEEE} International Conference on Robotics
 *                  and Automation, {ICRA} 2004, April 26 - May 1, 2004, New Orleans,
 *                  LA, {USA}},
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

#include <math.h>

class MultirotorDynamics {

    private:

        // Universal constants
        static constexpr double g  = 9.80665; // might want to allow this to vary!
        static constexpr double pi = 3.14159;

        // State variables: 0 = value; 1 = first derivative; 2 = second derivative
        double _x[3]     = {0};
        double _y[3]     = {0};
        double _z[3]     = {0};
        double _phi[3]   = {0};
        double _theta[3] = {0};
        double _psi[3]   = {0};

        // Set by subclass constructor
        int _nmotors;

        // Radians per second for each motor
        double * _omegas;

        // Flag for whether we're airborne
        bool _airborne = false;

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

        virtual double u2(double * o2)  = 0;
        virtual double u3(double * o2)  = 0;
        virtual double u4(double * o2)  = 0;

        virtual double omega(double * o) = 0;

         MultirotorDynamics(int nmotors)
        {
            _omegas = new double[nmotors];
        }

        virtual ~MultirotorDynamics(void)
        {
            delete _omegas;
        }

        // Values computed in Equation 6
        double _U1 = 0;
        double _U2 = 0;
        double _U3 = 0;
        double _U4 = 0;
        double _Omega = 0;

    public:

        /** 
         * Initializes pose, with flag for whether we're airbone (helps with testing gravity).
         */
        void init(double position[3], double rotation[3], bool airborne=false)
        {
            // Zero-out first and second derivatives in state
            for (int i=1; i<3; ++i) {
                _x[i]     = 0;
                _y[i]     = 0;
                _z[i]     = 0;
                _phi[i]   = 0;
                _theta[i] = 0;
                _psi[i]   = 0;
             }

            // Set pose
            _x[0]     = position[0];
            _y[0]     = position[1];
            _z[0]     = position[2];
            _phi[0]   = rotation[0];
            _theta[0] = rotation[1];
            _psi[0]   = rotation[2];

            // We can start on the ground (default) or in the air
            _airborne = airborne;
        }

        /** 
         * Updates state using Equation 5.
         * @param dt time in seconds since previous update
         */
        void update(double dt)
        {
            _x[2]     = (cos(_phi[0])*sin(_theta[0])*cos(_psi[0]) + sin(_phi[0])*sin(_psi[0])) / m() * _U1;

            _y[2]     = (cos(_phi[0])*sin(_theta[0])*sin(_psi[0]) + sin(_phi[0])*cos(_psi[0])) / m() * _U1;

            _z[2]     = -g + (cos(_phi[0])*cos(_theta[0])) / m() * _U1;

            _phi[2]   = _theta[1]*_psi[1]*(Iy()-Iz())/Ix() - Jr()/Ix()*_theta[1]*_Omega + l()/Ix()*_U2;

            _theta[2] = _phi[1]*_psi[1]*(Iz()-Ix())/Iy()   + Jr()/Iy()*_phi[1]*_Omega   + l()/Iy()*_U3; 

            _psi[2]   = _phi[1]*_theta[1]*(Ix()-Iy())/Iz() + _U4/Iz();
        }

        /**
         * Uses motor values to implement Equation 6.
         * @param motorvals in interval [0,1]
         */
        void setMotors(double * motorvals) 
        {
            // For any vehicle, U1 is always the scaled sum of the motor omegas
            _U1 = 0;

            // Convert the  motor values to radians per second, accumulating U1
            for (int i=0; i<_nmotors; ++i) {
                _omegas[i] = motorvals[i] * maxrpm() * pi / 30;
                _U1 += _omegas[i];
            }

            // Scale U1
            _U1 *= b();

            // Compute Omega from Omegas
            _Omega = omega(_omegas);

            // Square the Omegas
            for (int i=0; i<_nmotors; ++i) {
                _omegas[i] *= _omegas[i];
            }

            // Use the squared Omegas to implement the rest of Eqn. 6
            _U2 = b() * u2(_omegas);
            _U3 = b() * u3(_omegas);
            _U4 = d() * u4(_omegas);

        }

        /*
         *  Gets current state
         */
        void getState(double angularVelocity[3], double eulerAngles[3], double velocityXYZ[3], double positionXYZ[3])
        {
        }

        // Factory method
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics








