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
        FNullFlightManager(MultirotorDynamics * dynamics, int nmotors) 
            : FFlightManager(dynamics, nmotors) 
        {
        }

        virtual ~FNullFlightManager(void)
        {
        }

        virtual void getMotors(const double time, double * motorvals) override
        {
        }

}; // NullFlightManager
