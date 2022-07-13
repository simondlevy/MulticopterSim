#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include <datatypes.h>
#include <motor.h>

class FHackflightFlightManager : public FFlightManager {

    private:

        float _motorvals[MAX_SUPPORTED_MOTORS] = {};

        // Guards socket comms
        bool _ready = false;

        // PID controller for altitude hold
        altPid_t _alt_pid;

    protected:

        //virtual void getMotors(double time, double * values) override;

        virtual void getMotors(double time, double* values) override;

    public:

        FHackflightFlightManager(APawn * pawn, Dynamics * dynamics, mixer_t mixer);

        ~FHackflightFlightManager();

        void tick(void);

}; // FHackflightFlightManager
