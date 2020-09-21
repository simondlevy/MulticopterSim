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
		STATE_PSI_DOT
	};

	/**
	 * Exported state representations
	 */

	 // Kinematics
	typedef struct {

		double location[3];
		double rotation[3];

	} pose_t;

	// Dynamics
	typedef struct {

		double angularVel[3];
		double bodyAccel[3];
		double inertialVel[3];
		double quaternion[4];

		pose_t pose;

	} state_t;

	/**
	 * Class for parameters from the table below Equation 3
	 */
	class Parameters {

		friend class Dynamics;

	public:

		double b;
		double d;
		double m;
		double l;
		double Ix;
		double Iy;
		double Iz;
		double Jr;

		uint16_t maxrpm;

		Parameters(double b, double d, double m, double l, double Ix, double Iy, double Iz, double Jr, uint16_t maxrpm)
		{
			this->b = b;
			this->d = d;
			this->m = m;
			this->l = l;
			this->Ix = Ix;
			this->Iy = Iy;
			this->Iz = Iz;
			this->Jr = Jr;

			this->maxrpm = maxrpm;
		}
	};

protected:

	// Data structure for returning state
	state_t _state = {};

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

protected:

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

	// parameter block
	Parameters* _p = NULL;

	// roll right
	virtual double u2(double* o) = 0;

	// pitch forward
	virtual double u3(double* o) = 0;

	// yaw cw
	virtual double u4(double* o) = 0;

	// radians per second for each motor, and their squared values
	double* _omegas = NULL;
	double* _omegas2 = NULL;

	// quad, hexa, octo, etc.
	uint8_t _motorCount = 0;


	/**
	 *  Constructor
	 */
	Dynamics(Parameters* params, const uint8_t motorCount)
	{
		_p = params;

		_omegas = new double[motorCount]();
		_omegas2 = new double[motorCount]();

		_motorCount = motorCount;

		for (uint8_t i = 0; i < 12; ++i) {
			_x[i] = 0;
		}
	}

	virtual void updateGimbalDynamics(double dt) {}

	virtual void computeStateDerivative(double accelNED[3], double netz) = 0;

	virtual double computeMotorSpeed(double motorval) = 0;

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

	virtual void update(double dt) = 0;


	/**
	 * Returns state structure.
	 * @return state structure
	 */
	state_t getState(void)
	{
		return _state;
	}

	/**
	 * Returns "raw" state vector.
	 * @return state vector
	 */
	double* getStateVector(void)
	{
		return _x;
	}

	virtual void setMotors(double* motorvals, double dt) = 0;

	/**
	 *  Gets current pose
	 *
	 *  @return data structure containing pose
	 */
	pose_t getPose(void)
	{
		pose_t pose = {};

		for (uint8_t i = 0; i < 3; ++i) {
			uint8_t ii = 2 * i;
			pose.rotation[i] = _x[STATE_PHI + ii];
			pose.location[i] = _x[STATE_X + ii];
		}

		return pose;
	}

	/**
	 * Sets height above ground level (AGL).
	 * This method can be called by the kinematic visualization.
	 */
	void setAgl(double agl)
	{
		_agl = agl;
	}

	// Motor direction for animation
	virtual int8_t motorDirection(uint8_t i) { (void)i; return 0; }

	/**
	 *  Frame-of-reference conversion routines.
	 *
	 *  See Section 5 of http://www.chrobotics.com/library/understanding-euler-angles
	 */

	static void bodyToInertial(double body[3], const double rotation[3], double inertial[3])
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

		double R[3][3] = { {cps * cth,  cps * sph * sth - cph * sps,  sph * sps + cph * cps * sth},
			{cth * sps,  cph * cps + sph * sps * sth,  cph * sps * sth - cps * sph},
			{-sth,     cth * sph,                cph * cth} };

		dot(R, body, inertial);
	}

	static void inertialToBody(double inertial[3], const double rotation[3], double body[3])
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

		double R[3][3] = { {cps * cth,                cth * sps,                   -sth},
			{cps * sph * sth - cph * sps,  cph * cps + sph * sps * sth,  cth * sph},
			{sph * sps + cph * cps * sth,  cph * sps * sth - cps * sph,  cph * cth} };

		dot(R, inertial, body);
	}

	/**
	 * Converts Euler angles to quaterion.
	 *
	 * @param eulerAngles input
	 * @param quaternion output
	 */

	static void eulerToQuaternion(const double eulerAngles[3], double quaternion[4])
	{
		// Convenient renaming
		double phi = eulerAngles[0] / 2;
		double the = eulerAngles[1] / 2;
		double psi = eulerAngles[2] / 2;

		// Pre-computation
		double cph = cos(phi);
		double cth = cos(the);
		double cps = cos(psi);
		double sph = sin(phi);
		double sth = sin(the);
		double sps = sin(psi);

		// Conversion
		quaternion[0] = cph * cth * cps + sph * sth * sps;
		quaternion[1] = cph * sth * sps - sph * cth * cps;
		quaternion[2] = -cph * sth * cps - sph * cth * sps;
		quaternion[3] = cph * cth * sps - sph * sth * cps;
	}

	/**
	 * Gets motor count set by constructor.
	 * @return motor count
	 */
	uint8_t motorCount(void)
	{
		return _motorCount;
	}

}; // class Dynamics
