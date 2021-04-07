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
        FNullFlightManager(Dynamics * dynamics, int nmotors) 
            : FFlightManager(dynamics, nmotors) 
        {
        }

        virtual ~FNullFlightManager(void)
        {
        }

        virtual void getMotors(const double time, double * motorvals) override
        {
            float spin = 0.6;
            motorvals[0] = spin;
            motorvals[1] = spin;
            motorvals[2] = spin;
            motorvals[3] = spin;
        }

}; // NullFlightManager
