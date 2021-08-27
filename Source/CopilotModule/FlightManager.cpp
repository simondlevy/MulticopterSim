/*
   MulticopterSim FlightManager class implementation using Haskell Copilot

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "FlightManager.h"

#include "hackflight.h"

// Used by Copilot ---------------------------------

double copilot_time = 0;

double copilot_receiverThrottle = 0;
double copilot_receiverRoll = 0;
double copilot_receiverPitch = 0;
double copilot_receiverYaw = 0;

double copilot_altimeterZ = 0;
double copilot_altimeterDz = 0;

double copilot_gyrometerX = 0;
double copilot_gyrometerY = 0;
double copilot_gyrometerZ = 0;

double copilot_quaternionW = 0;
double copilot_quaternionX = 0;
double copilot_quaternionY = 0;
double copilot_quaternionZ = 0;

// Sent by Copilot to copilot_runMotors() -----------
static double _m1;
static double _m2;
static double _m3;
static double _m4;

// Called by Copilot
void copilot_runMotors(double m1, double m2, double m3, double m4)
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
    copilot_quaternionW = 0;
    copilot_quaternionX = 0;
    copilot_quaternionY = 0;
    copilot_quaternionZ = 0;
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
