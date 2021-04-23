/*
   MulticopterSim FlightManager class implementation using a stub

   Rises to a few meters then cuts motors

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"

class FNullFlightManager : public FFlightManager {

    public:

        // Constructor
        FNullFlightManager(Dynamics * dynamics) 
            : FFlightManager(dynamics) 
        {
        }

        virtual ~FNullFlightManager(void)
        {
        }

        virtual void getMotors(const double time, double * motorvals) override
        {
            for (uint8_t k=0; k<_nmotors; ++k) {
                motorvals[k] = 0.6;
            }
        }

}; // NullFlightManager
