/*
   MulticopterSim FlightManager class implementation using Haskell Copilot

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "FlightManager.h"

#include "hackflight.h"

// Used by Copilot ---------------------------------

float copilot_time = 0;

float copilot_receiverThrottle = 0;
float copilot_receiverRoll = 0;
float copilot_receiverPitch = 0;
float copilot_receiverYaw = 0;

float copilot_altimeterZ = 0;
float copilot_altimeterDz = 0;

float copilot_gyrometerX = 0;
float copilot_gyrometerY = 0;
float copilot_gyrometerZ = 0;

float copilot_quaternionW = 0;
float copilot_quaternionX = 0;
float copilot_quaternionY = 0;
float copilot_quaternionZ = 0;

float copilot_flowX = 0;
float copilot_flowY = 0;

// Sent by Copilot to copilot_runMotors() -----------
static float _m1;
static float _m2;
static float _m3;
static float _m4;

// Called by Copilot
void copilot_runMotors(float m1, float m2, float m3, float m4)
{
    _m1 = m1;
    _m2 = m2;
    _m3 = m3;
    _m4 = m4;
}


FCopilotFlightManager::FCopilotFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    _gameInput = new GameInput(pawn);

    _connected = true;
}

FCopilotFlightManager::~FCopilotFlightManager()
{
}

void FCopilotFlightManager::getReceiverDemands(void)
{
    // Get stick demands
    _gameInput->getJoystick(_joyvals);

    // Share the stick demands
    copilot_receiverThrottle = _joyvals[0];
    copilot_receiverRoll     = _joyvals[1];
    copilot_receiverPitch    = _joyvals[2];
    copilot_receiverYaw      = _joyvals[3];
}


void FCopilotFlightManager::getGyrometer(void)
{
    copilot_gyrometerX = _dynamics->x(Dynamics::STATE_PHI_DOT); 
    copilot_gyrometerY = _dynamics->x(Dynamics::STATE_THETA_DOT); 
    copilot_gyrometerZ = _dynamics->x(Dynamics::STATE_PSI_DOT); 
}

void FCopilotFlightManager::getQuaternion(void)
{
    double phi   = _dynamics->x(Dynamics::STATE_PHI); 
    double theta = _dynamics->x(Dynamics::STATE_THETA); 
    double psi   = _dynamics->x(Dynamics::STATE_PSI); 

    // Pre-computation
    double cph = cos(phi);
    double cth = cos(theta);
    double cps = cos(psi);
    double sph = sin(phi);
    double sth = sin(theta);
    double sps = sin(psi);

    copilot_quaternionW = cph * cth * cps + sph * sth * sps;
    copilot_quaternionX = cph * sth * sps - sph * cth * cps;
    copilot_quaternionY = -cph * sth * cps - sph * cth * sps;
    copilot_quaternionZ = cph * cth * sps - sph * sth * cps;
}

void FCopilotFlightManager::getOpticalFlow(void)
{
    double dx = _dynamics->x(Dynamics::STATE_X_DOT);
    double dy = _dynamics->x(Dynamics::STATE_Y_DOT);

    debugline("DX: %+3.3f  DY: %+3.3f", dx, dy);

    // XXX ignore pitch, roll influence for now
    copilot_flowX = 0;
    copilot_flowY = 0;
}

void FCopilotFlightManager::getActuators(const double time, double * values)
{
    // Avoid null-pointer exceptions at startup, freeze after control
    // program halts
    if (!_connected) {
        return;
    }

    // Share the current time with Copilot
    copilot_time = time; 

    // Share stick demands with Copilot
    getReceiverDemands();

    // Share the gyrometer values
    getGyrometer();

    // Share the quaternion values
    getQuaternion();

    // Share the optical flow values
    getOpticalFlow();

    // Share the altimeter value
    copilot_altimeterZ = _dynamics->x(Dynamics::STATE_Z); 

    // Run Copilot, triggering copilot_runMotors
    step();

    // Get updated motor values
    values[0] = _m1;
    values[1] = _m2;
    values[2] = _m3;
    values[3] = _m4;
}

void FCopilotFlightManager::tick(void)
{
    // Get demands from keypad
    _gameInput->getKeypad(_joyvals);
}
