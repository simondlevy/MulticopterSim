#define WIN32_LEAN_AND_MEAN

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"
#include "../MainModule/Joystick.h"

class FRustFlightManager : public FFlightManager {

    private:

        typedef struct {

            float throttle;
            float roll;
            float pitch;
            float yaw;

        } demands_t;

        typedef struct {

            float m1;
            float m2;
            float m3;
            float m4;

        } motors_t;

        typedef struct {

            float altitude_target;
            float error_integral;
            float throttle_demand;

        } alt_hold_t;

        typedef struct {

            demands_t demands;
            Dynamics::vehicle_state_t vehicle_state;
            alt_hold_t alt_hold;
            motors_t motors;

        } hackflight_t;

        typedef hackflight_t (*run_hackflight_t) (
                demands_t * demands,
                Dynamics::vehicle_state_t * vehicle_state,
                alt_hold_t * alt_hold
                );

        run_hackflight_t _run_hackflight;

        Dynamics * _dynamics;

        IJoystick * _joystick;

        hackflight_t _hackflight;

        static void run_hackflight(hackflight_t * hackflight);

        static void run_alt_hold(
                float throttle,
                float z,
                float dz,
                alt_hold_t * alt_hold);

    protected:

        virtual void getMotors(double time, double* values) override;

    public:

        FRustFlightManager(APawn * pawn, Dynamics * dynamics);

        ~FRustFlightManager();

        void tick(void);

}; // FRustFlightManager
