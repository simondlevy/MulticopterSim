/*
   MulticopterSim FlightManager class using Hackflight

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

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
