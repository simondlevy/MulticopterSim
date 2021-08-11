/*
   MulticopterSim FlightManager class using Hackflight

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include <HF_pure.hpp>

// PID controllers
#include <hf_pidcontrollers/level.hpp>
#include <hf_pidcontrollers/rate.hpp>
#include <hf_pidcontrollers/yaw.hpp>
#include <hf_pidcontrollers/althold.hpp>
#include <hf_pidcontrollers/poshold.hpp>

#include "SimReceiver.hpp"
#include "SimBoard.hpp"
#include "SimSensors.hpp"
#include "SimMotor.hpp"

class FHackflightFlightManager : public FFlightManager {

    public:

        // Constructor
        FHackflightFlightManager(APawn * pawn, Dynamics * dynamics) 
            : FFlightManager(dynamics) 
        {

        }

        ~FHackflightFlightManager(void)
        {
        }

        // Runs on fast thread
        virtual void getActuators(const double time, double * values) override
        {
        }

        void tick(void)
        {
        }

}; // HackflightFlightManager
