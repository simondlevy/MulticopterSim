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

int dbg_nmotors = 0;

class NewMultirotorDynamics {

    //private:
    public:

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
        unsigned int _nmotors;

        // Values computed in Equation 6
        double _U1 = 0;
        double _U2 = 0; double _U3 = 0;
        double _U4 = 0;
        double _Omega = 0;

        // Radians per second for each motor
        double * _omegas;

        // Earth-frame acceleration
        double _earthFrameAcceleration[3] = {0};

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
        NewMultirotorDynamics(unsigned int nmotors)
        {
            _omegas = new double[nmotors];
            _nmotors = nmotors;
        }

    public:

        /**
         *  Destructor
         */
        virtual ~NewMultirotorDynamics(void)
        {
            delete _omegas;
        }

        /** 
         * Initializes pose, with flag for whether we're airbone (helps with testing gravity).
         *
         * @param location X,Y,Z
         * @param rotation phi, theta, psi
         * @param airborne allows us to start on the ground (default) or in the air (e.g., gravity test)
         */
        void init(double location[3], double rotation[3], bool airborne=false)
        {
            // Zero-out entire state
            for (int i=0; i<12; ++i) {
                _x[i] = 0;
            }

            _x[STATE_X] = location[0];
            _x[STATE_Y] = location[1];
            _x[STATE_Z] = location[2];

            _x[STATE_PHI]   = rotation[0];
            _x[STATE_THETA] = rotation[1];
            _x[STATE_PSI]   = rotation[2];

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
            // Temporal first derivative of state vector
            double dxdt[12] = {

                // Equation 12: replace state components with their first derivatives
                _x[1],
                (cos(_x[6])*sin(_x[8])*cos(_x[10]) + sin(_x[6])*sin(_x[10])) * _U1 / m(),
                _x[3],
                (cos(_x[6])*sin(_x[8])*sin(_x[10]) + sin(_x[6])*cos(_x[10])) * _U1 / m(),
                _x[5],
                -g + (cos(_x[6])*cos(_x[8])) * _U1 / m(),
                _x[7],
                _x[11]*_x[9]*(Iy()-Iz())/Ix() - Jr()/Ix()*_x[9]*_Omega + l()/Ix()*_U2,
                _x[9],
                _x[11]*_x[7]*(Iz()-Ix())/Iy()   + Jr()/Iy()*_x[7]*_Omega   + l()/Iy()*_U3, 
                _x[11],
                _x[9]*_x[7]*(Ix()-Iy())/Iz() + l()/Iz()*_U4,
            };

            // Compute temporal first integral of state vector
            for (int i=0; i<12; ++i) {
                _x[i] += dt * dxdt[i];
            }

            // Store earth-frame acceleration for simulating accelerometer
            _earthFrameAcceleration[0] = dxdt[1];
            _earthFrameAcceleration[1] = dxdt[3];
            _earthFrameAcceleration[2] = dxdt[5];
        }

        /**
         * Uses motor values to implement Equation 6.
         *
         * @param motorvals in interval [0,1]
         */
        void setMotors(double * motorvals) 
        {

            dbg_nmotors = _nmotors;
            return;

            // Convert the  motor values to radians per second
            for (unsigned int i=0; i<_nmotors; ++i) {
                _omegas[i] = motorvals[i] * maxrpm() * pi / 30;
            }

            // Compute Omega from Omegas
            _Omega = omega(_omegas);

            // U1 = sum of squared omegas
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
         *  @param angularVelocity
         *  @param earthFrameAcceleration
         *  @param eulerAngles
         *  @param velocityXYZ
         *  @param locationXYZ
         */
        void getState(
                double angularVelocity[3], 
                double earthFrameAcceleration[3], 
                double eulerAngles[3], 
                double velocityXYZ[3], 
                double locationXYZ[3])
        {
            earthFrameAcceleration[0] = _earthFrameAcceleration[0];
            earthFrameAcceleration[1] = _earthFrameAcceleration[1];
            earthFrameAcceleration[2] = _earthFrameAcceleration[2];

            angularVelocity[0] = _x[STATE_PHI_DOT];
            angularVelocity[1] = _x[STATE_THETA_DOT];
            angularVelocity[2] = _x[STATE_PSI_DOT];

            eulerAngles[0] = _x[STATE_PHI];
            eulerAngles[1] = _x[STATE_THETA];
            eulerAngles[2] = _x[STATE_PSI];

            locationXYZ[0] = _x[STATE_X];
            locationXYZ[1] = _x[STATE_Y];
            locationXYZ[2] = _x[STATE_Z];

            velocityXYZ[0] = _x[STATE_X_DOT];
            velocityXYZ[1] = _x[STATE_Y_DOT];
            velocityXYZ[2] = _x[STATE_Z_DOT];
          }

        /**
         * Factory method
         */
        static NewMultirotorDynamics * create(void);

}; // class MultirotorDynamics








