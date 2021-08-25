/*

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
    /*
    _m1 = m1;
    _m2 = m2;
    _m3 = m3;
    _m4 = m4;
    */
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

    // Remaining values are stick demands
    _gameInput->getJoystick(_joyvals);
}

void FCopilotFlightManager::tick(void)
{
    // Get demands from keypad
    _gameInput->getKeypad(_joyvals);
}
