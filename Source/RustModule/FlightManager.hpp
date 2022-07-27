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

        typedef struct {

            float throttle;
            float roll;
            float pitch;
            float yaw;

        } demands_t;

        typedef alt_hold_t (*alt_hold_fun_t) (
                demands_t * demands,
                Dynamics::vehicle_state_t * vehicle_state,
                alt_hold_t * oldpid
                );

        alt_hold_fun_t _run_alt_hold;

        Dynamics * _dynamics;

        IJoystick * _joystick;

        static float scaleAxis(float value);

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
