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

class MultirotorDynamics {

    private:

        // Universal constants
        static constexpr double g  = 9.80665; // might want to allow this to vary!
        static constexpr double pi = 3.14159;

        // State vector (see Eqn. 12)
        double _x[12] = {0};


        // Set by subclass constructor
        int _nmotors;

        // Values computed in Equation 6
        double _U1 = 0;
        double _U2 = 0;
        double _U3 = 0;
        double _U4 = 0;
        double _Omega = 0;

        // Radians per second for each motor
        double * _omegas;

        // Flag for whether we're airborne
        bool _airborne = false;

    protected:

        /** 
         * You must implement these methods in a subclass for each vehicle.
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

        /**
         *  Constructor
         */
        MultirotorDynamics(int nmotors)
        {
            _omegas = new double[nmotors];
        }

        /**
         *  Destructor
         */
         virtual ~MultirotorDynamics(void)
        {
            delete _omegas;
        }

    public:

        /** 
         * Initializes pose, with flag for whether we're airbone (helps with testing gravity).
         *
         * @param position X,Y,Z
         * @param rotation phi, theta, psi
         * @param airborne allows us to start on the ground (default) or in the air (e.g., gravity test)
         */
        void init(double position[3], double rotation[3], bool airborne=false)
        {
            // Zero-out entire state
            for (int i=0; i<12; ++i) {
                _x[i] = 0;
             }

            // Set pose
            _x[0]  = position[0];
            _x[2]  = position[1];
            _x[4]  = position[2];
            _x[6]  = rotation[0];
            _x[8]  = rotation[1];
            _x[10] = rotation[2];

            // We can start on the ground (default) or in the air
            _airborne = airborne;
        }

        /** 
         * Updates state.
         *
         * @param dt time in seconds since previous update
         */
        void update(double dt)
        {
            // Equation 12
            _x[0]  = _x[1];
            _x[1]  = (cos(_x[6])*sin(_x[8])*cos(_x[10]) + sin(_x[6])*sin(_x[10])) * _U1 / m();
            _x[2]  = _x[3];
            _x[3]  = (cos(_x[6])*sin(_x[8])*sin(_x[10]) + sin(_x[6])*cos(_x[10])) * _U1 / m();
            _x[4]  = _x[5];
            _x[5]  = -g + (cos(_x[6])*cos(_x[8])) * _U1 / m();
            _x[6]  = _x[7];
            _x[7]  = _x[11]*_x[9]*(Iy()-Iz())/Ix() - Jr()/Ix()*_x[9]*_Omega + l()/Ix()*_U2;
            _x[8]  = _x[9];
            _x[9]  = _x[11]*_x[7]*(Iz()-Ix())/Iy()   + Jr()/Iy()*_x[7]*_Omega   + l()/Iy()*_U3; 
            _x[10] = _x[11];
            _x[11] = _x[9]*_x[7]*(Ix()-Iy())/Iz() + l()/Iz()*_U4;
        }

        /**
         * Uses motor values to implement Equation 6.
         *
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
         *
         *  @param angularVelocity
         *  @param eulerAngles
         *  @param velocityXYZ
         *  @param positionXYZ
         */
        void getState(double angularVelocity[3], double eulerAngles[3], double velocityXYZ[3], double positionXYZ[3])
        {
        }

        /**
         * Factory method
         */
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics








