/*
   MulticopterSim FlightManager class implementation using Haskell Copilot

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "FlightManager.h"

#include "hackflight.h"

// Used by Copilot
double copilot_receiverThrottle = 0;
double copilot_receiverRoll = 0;
double copilot_receiverPitch = 0;
double copilot_receiverYaw = 0;

double copilot_altimeterZ = 0;

double copilot_time = 0;

double copilot_gyrometerX = 0;
double copilot_gyrometerY = 0;
double copilot_gyrometerZ = 0;

double copilot_quaternionW = 0;
double copilot_quaternionX = 0;
double copilot_quaternionY = 0;
double copilot_quaternionZ = 0;

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
}

FCopilotFlightManager::~FCopilotFlightManager()
{
}

void FCopilotFlightManager::getActuators(const double time, double * values)
{
    // Get the "receiver" values
    double joyvals[4] = {};
    _gameInput->getJoystick(joyvals);
    copilot_receiverThrottle = joyvals[0];
    copilot_receiverRoll     = joyvals[1];
    copilot_receiverPitch    = joyvals[2];
    copilot_receiverYaw      = joyvals[3];


    // Get the sensor values


    // Call Copilot, which will call copilot_runMotors()
    step();

    // Fill the motor values with values from copilot_runMotors()
    values[0] = _m1;
    values[1] = _m2;
    values[2] = _m3;
    values[3] = _m4;

}
