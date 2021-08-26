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

double copilot_gyrometerX = 0;
double copilot_gyrometerY = 0;
double copilot_gyrometerZ = 0;

// Sent by Copilot to copilot_runMotors() -----------
static double _m1 = 0.6;
static double _m2 = 0.6;
static double _m3 = 0.6;
static double _m4 = 0.6;

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

void FCopilotFlightManager::getActuators(const double time, double * values)
{
    // Avoid null-pointer exceptions at startup, freeze after control
    // program halts
    if (!_connected) {
        return;
    }

    // Get stick demands
    _gameInput->getJoystick(_joyvals);

    // Share the current time with Copilot
    copilot_time = time; 

    // Share the stick demands
    copilot_receiverThrottle = _joyvals[0];
    copilot_receiverRoll     = _joyvals[1];
    copilot_receiverPitch    = _joyvals[2];
    copilot_receiverYaw      = _joyvals[3];

    // Share the gyrometer values
    copilot_gyrometerX = _dynamics->x(Dynamics::STATE_PHI_DOT); 
    copilot_gyrometerY = _dynamics->x(Dynamics::STATE_THETA_DOT); 
    copilot_gyrometerZ = _dynamics->x(Dynamics::STATE_PSI_DOT); 

    // Share the altimeter value, negating for NED
    copilot_altimeterZ = -_dynamics->x(Dynamics::STATE_Z); 

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
