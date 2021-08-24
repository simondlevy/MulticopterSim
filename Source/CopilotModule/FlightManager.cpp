/*
   MulticopterSim FlightManager class implementation using Haskell Copilot

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include "FlightManager.h"

#include "hackflight.h"

double copilot_receiverThrottle = 0;
double copilot_altimeterZ = 0;
double copilot_time = 0;
double copilot_gyrometerX = 0;
double copilot_receiverRoll = 0;
double copilot_gyrometerY = 0;
double copilot_receiverPitch = 0;
double copilot_receiverYaw = 0;
double copilot_gyrometerZ = 0;
double copilot_quaternionW = 0;
double copilot_quaternionX = 0;
double copilot_quaternionY = 0;
double copilot_quaternionZ = 0;

void copilot_runMotors(double m1, double m2, double m3, double m4)
{
}

FCopilotFlightManager::FCopilotFlightManager(Dynamics * dynamics)
    : FFlightManager(dynamics)
{
}

FCopilotFlightManager::~FCopilotFlightManager()
{
}

void FCopilotFlightManager::getActuators(const double time, double * values)
{
    for (uint8_t i=0; i<_dynamics->rotorCount(); ++i) {
        values[i] = 0.6;
    }
}
