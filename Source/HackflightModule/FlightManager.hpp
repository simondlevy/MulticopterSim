#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include <pids/althold_struct.h>

class FHackflightFlightManager : public FFlightManager {

    private:

        float _motorvals[100] = {};

        // Guards socket comms
        bool _ready = false;

        // PID controller for altitude hold
        alt_pid_t _alt_pid;

    public:

        FHackflightFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FHackflightFlightManager();

        virtual void getMotors(const double time, double * values) override;

        void tick(void);

}; // FHackflightFlightManager
