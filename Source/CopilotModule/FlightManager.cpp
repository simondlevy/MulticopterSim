/*

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "FlightManager.h"

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
    _gameInput->getJoystick(&_telemetry[13]);
}

void FCopilotFlightManager::tick(void)
{
    // Get demands from keypad
    _gameInput->getKeypad(&_telemetry[13]);
}
