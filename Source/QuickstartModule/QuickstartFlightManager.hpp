/*
   Simple MulticopterSim FlightManager class implementation

   Copyright(C) 2021 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

class FQuickstartFlightManager : public FFlightManager {

    public:

        FQuickstartFlightManager(Dynamics * dynamics) : 
            FFlightManager(dynamics)
        {
        }
		
        ~FQuickstartFlightManager()
        {
        }

        virtual void getMotors(const double time, double * motorvals) override
        {

            for (uint8_t i=0; i<_nmotors; ++i) {
                motorvals[i] = 0.6;
            }
        }

}; // FQuickstartFlightManager
