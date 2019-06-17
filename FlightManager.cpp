/*
   MulticopterSim FlightManager class implementation using a stub

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MulticopterSim/FlightManager.hpp"

class FNullFlightManager : public FFlightManager {

    public:

        // Constructor
        FNullFlightManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation) 
            : FFlightManager(dynamics, initialLocation, initialRotation) 
        {
        }

        virtual ~FNullFlightManager(void)
        {
        }

        virtual void update(const double time, const MultirotorDynamics::state_t & state, double * motorvals) override
        {
            motorvals[0] = 0.1;
        }

}; // NullFlightManager


// Factory method for FlightManager class
SIMPLUGIN_API FFlightManager * createFlightManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation)
{
    return new FNullFlightManager(dynamics, initialLocation, initialRotation);
}
