#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"
#include "../MainModule/Joystick.h"

class FRustFlightManager : public FFlightManager {

    private:

        typedef struct {

            float error_integral;
            bool  in_band;
            float target;
            float throttle;

        } alt_hold_t;

        typedef alt_hold_t (*alt_hold_fun_t) (
                float throttle,
                float altitude,
                float climb_rate,
                alt_hold_t * oldpid
                );

        alt_hold_fun_t _run_alt_hold;

        static void alt_hold(
                float throttle,
                float altitude,
                float climb_rate,
                alt_hold_t * oldpid,
                alt_hold_t * newpid);

        Dynamics * _dynamics;

        IJoystick * _joystick;

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
