/*
   MulticopterSim FlightManager class implementation using Haskell Copilot

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include "hackflight.h"


class FCopilotFlightManager : public FFlightManager {


    public:

        FCopilotFlightManager(Dynamics * dynamics);

        ~FCopilotFlightManager();

        virtual void getActuators(const double time, double * values) override;

}; // FCopilotFlightManager
