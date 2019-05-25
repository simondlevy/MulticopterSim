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

class MultirotorDynamics {

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
        unsigned int _nmotors = 0;

        // Values computed in Equation 6
        double _U1 = 0;     // total thrust
        double _U2 = 0;     // roll thrust right
        double _U3 = 0;     // pitch thrust forward
        double _U4 = 0;     // yaw thrust clockwise
        double _Omega = 0;  // torque clockwise

        // Radians per second for each motor
        double * _omegas = NULL;

        // Earth-frame acceleration
        double _inertialAcc[3] = {0};

        // Flag for whether we're airborne
        bool _airborne = false;

        double z_dot_dot = 0;

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
        MultirotorDynamics(unsigned int nmotors)
        {
            _omegas = new double[nmotors];
            _nmotors = nmotors;
        }

    public:

        /**
         *  Destructor
         */
        virtual ~MultirotorDynamics(void)
        {
            delete _omegas;
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
            // Zero-out entire state
            for (int i=0; i<12; ++i) {
                _x[i] = 0;
            }

            // Initialize pose
            for (int i=0; i<3; ++i) {
                _x[STATE_X+i] = location[i];
                _x[STATE_PHI+i] = rotation[i];
            }

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
            // Compute net vertical acceleration using Equation 12,
            // then negate to accommodate NED coordinates
            z_dot_dot = g - (cos(_x[6])*cos(_x[8])) * _U1 / m();

            // We're airborne once net vertical acceleration falls below zero
            if (!_airborne) {
                _airborne = z_dot_dot < 0;
            }

            // Once airborne, we can update dynamics
            if (_airborne) {

                double dxdt[12] = {

                    // Equation 12: compute temporal first derivative of state.
                    // We negate x'' and theta'' to accommodate NED coordinates
                    /* x'      */ _x[1],
                    /* x''     */ -((cos(_x[6])*sin(_x[8])*cos(_x[10]) + sin(_x[6])*sin(_x[10]))) * _U1 / m(),
                    /* y'      */ _x[3],
                    /* y''     */ (cos(_x[6])*sin(_x[8])*sin(_x[10]) + sin(_x[6])*cos(_x[10])) * _U1 / m(),
                    /* z'      */ _x[5],
                    /* z''     */ z_dot_dot,
                    /* phi'    */ _x[7],
                    /* phi''   */ _x[11]*_x[9]*(Iy()-Iz())/Ix() - Jr()/Ix()*_x[9]*_Omega + l()/Ix()*_U2,
                    /* theta'  */ _x[9],
                    /* theta'' */ -(_x[11]*_x[7]*(Iz()-Ix())/Iy()   + Jr()/Iy()*_x[7]*_Omega   + l()/Iy()*_U3), 
                    /* psi'    */ _x[11],
                    /* psi''   */ _x[9]*_x[7]*(Ix()-Iy())/Iz() + l()/Iz()*_U4,
                };

                // Compute state as first temporal integral of first temporal derivative
                for (int i=0; i<12; ++i) {
                    _x[i] += dt * dxdt[i];
                }

                // Store earth-frame acceleration for simulating accelerometer
                _inertialAcc[0] = dxdt[STATE_X_DOT];
                _inertialAcc[1] = dxdt[STATE_Y_DOT];
                _inertialAcc[2] = dxdt[STATE_Z_DOT];
            }
        }

        /**
         * Uses motor values to implement Equation 6.
         *
         * @param motorvals in interval [0,1]
         */
        void setMotors(double * motorvals) 
        {
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
         *  @param angularVel  angular velocity radians / second, in body frame
         *  @param inertialAcc linear acceleration in inertial frame
         *  @param rotation    Euler angles in radians
         *  @param inertialVel linear velocity in inertial frame
         *  @param location    location in inertial frame
         */
        void getState(double angularVel[3], double inertialAcc[3], double rotation[3], double inertialVel[3], double location[3])
        {
            for (int i=0; i<3; ++i) {
                inertialAcc[i] = _inertialAcc[i];
                angularVel[i]  = _x[STATE_PHI_DOT+2*i];
                rotation[i]    = _x[STATE_PHI+2*i];
                location[i]    = _x[STATE_X+2*i];
                inertialVel[i] = _x[STATE_X_DOT+2*i];
            }
        }

        /**
         * Factory method
         */
        static MultirotorDynamics * create(void);

}; // class MultirotorDynamics
