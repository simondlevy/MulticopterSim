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

        // 

        int _nmotors;

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

        // Functions computing these values for a particular build
        virtual double u2(double * o2)  = 0;
        virtual double u3(double * o2)  = 0;
        virtual double u4(double * o2)  = 0;
        virtual double omega(double * o) = 0;
 
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

            // We can start on the ground (default) or in the air
            _airborne = airborne;
        }

        /** 
         * Updates dynamics state.
         */
        void update(double dt)
        {
            // Use scaled forces to update state second-derivatives (Eqn. 6)
            _xdd = (cos(_phi)*sin(_theta)*cos(_psi) + sin(_phi)*sin(_psi)) / m() * _U1;
            _ydd = (cos(_phi)*sin(_theta)*sin(_psi) + sin(_phi)*cos(_psi)) / m() * _U1;
            _zdd = -g + (cos(_phi)*cos(_theta)) / m() * _U1;
        }

        /**
         * Uses motor values to implement Equations 5 and 6.
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








