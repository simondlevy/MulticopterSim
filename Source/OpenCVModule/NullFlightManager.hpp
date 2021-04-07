/*
   MulticopterSim FlightManager class implementation using a stub

   Rises to a few meters then cuts motors

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"

class FNullFlightManager : public FFlightManager {

    private:

        bool _running = false;

    public:

        // Constructor
        FNullFlightManager(MultirotorDynamics * dynamics) 
            : FFlightManager(dynamics) 
        {
            _running = true;
        }

        virtual ~FNullFlightManager(void)
        {
        }

        virtual void getMotors(const double time, const MultirotorDynamics::state_t & state, double * motorvals) override
        {
            bool high = state.pose.location[2] < -3; // NED!

            if (high) {
                _running = false;
            }

            for (uint8_t i=0; i<_motorCount; ++i) {
                //motorvals[i] = _running ? 0.6 : 0;
                motorvals[i] = high ? 0 : 0.6;
            }
        }

}; // NullFlightManager
