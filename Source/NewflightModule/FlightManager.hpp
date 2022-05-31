#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

class FNewflightFlightManager : public FFlightManager {

    private:

        Dynamics * _dynamics;

        float _motorvals[100] = {};

        // Guards socket comms
        bool _ready = false;

    public:

        FNewflightFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FNewflightFlightManager();

        virtual void getActuators(const double time, double * values) override;

        void tick(void);

}; // FNewflightFlightManager
