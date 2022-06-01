#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

class FHackflightFlightManager : public FFlightManager {

    private:

        Dynamics * _dynamics;

        float _motorvals[100] = {};

        // Guards socket comms
        bool _ready = false;

    public:

        FHackflightFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FHackflightFlightManager();

        virtual void getActuators(const double time, double * values) override;

        void tick(void);

}; // FHackflightFlightManager
