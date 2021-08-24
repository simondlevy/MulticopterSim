/*
   MulticopterSim FlightManager class implementation using Haskell Copilot

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

class FCopilotFlightManager : public FFlightManager {

    private:


    public:

        FCopilotFlightManager(Dynamics * dynamics)
            : FFlightManager(dynamics)
        {
        }

        ~FCopilotFlightManager()
        {
        }

        virtual void getActuators(const double time, double * values) override
        {
            for (uint8_t i=0; i<_dynamics->rotorCount(); ++i) {
                values[i] = 0.6;
            }
        }

}; // FCopilotFlightManager
