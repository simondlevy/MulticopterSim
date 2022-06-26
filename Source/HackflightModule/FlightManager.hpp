#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include <motor.h>
#include <pids/althold_struct.h>

class FHackflightFlightManager : public FFlightManager {

    private:

        float _motorvals[MAX_SUPPORTED_MOTORS] = {};

        // Guards socket comms
        bool _ready = false;

        // PID controller for altitude hold
        alt_pid_t _alt_pid;

    public:

        FHackflightFlightManager(APawn * pawn, Dynamics * dynamics, mixer_t mixer);

        ~FHackflightFlightManager();

        virtual void getMotors(const double time, double * values) override;

        void tick(void);

}; // FHackflightFlightManager
