#include "FlightManager.hpp"
#include "../MainModule/Utils.hpp"

#include <SDL.h>

static const char * LIBRARY_FILENAME = "hackflight.dll";

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    _dynamics = dynamics;

    void * library_handle = SDL_LoadObject(LIBRARY_FILENAME);

    _run_hackflight =
        (run_hackflight_t) SDL_LoadFunction(library_handle, "rust_run_hackflight");

    _joystick = new IJoystick();
}

FRustFlightManager::~FRustFlightManager()
{
}

static float constrain(float v, float lo, float hi)
{
    return v < lo ? lo : v > hi ? hi : v;
}

static float constrain_abs(float v, float lim)
{
    return constrain(v, -lim, +lim);
}

typedef struct {

    bool  in_band;
    float error_integral;
    float target;
    float throttle;

} alt_hold_pid_t;

static void alt_hold(
        float throttle,
        float altitude,
        float climb_rate,
        alt_hold_pid_t * oldpid,
        alt_hold_pid_t * newpid)
{
    static constexpr float KP = 0.75;
    static constexpr float KI = 1.5;
    static constexpr float ALTITUDE_MIN   = 1.0;
    static constexpr float PILOT_VELZ_MAX = 2.5;
    static constexpr float STICK_DEADBAND = 0.2;
    static constexpr float WINDUP_MAX     = 0.4;

    // Rescale throttle [0,1] => [-1,+1]
    float sthrottle = 2 * throttle - 1; 

    // Is stick demand in deadband, above a minimum altitude?
    bool in_band = fabs(sthrottle) < STICK_DEADBAND && altitude > ALTITUDE_MIN; 

    // Zero throttle will reset error integral
    bool at_zero_throttle = throttle == 0;

    // Reset controller when moving into deadband above a minimum altitude
    bool gotNewTarget = in_band && !oldpid->in_band;
    newpid->error_integral = gotNewTarget || at_zero_throttle ? 0 : oldpid->error_integral;

    float altitude_target = at_zero_throttle ? 0 : oldpid->target;

    newpid->target = gotNewTarget ? altitude : altitude_target;

    // Target velocity is a setpoint inside deadband, scaled
    // constant outside
    float target_velocity = in_band ?
        newpid->target - altitude :
        PILOT_VELZ_MAX * sthrottle;

    // Compute error as scaled target minus actual
    float error = target_velocity - climb_rate;

    // Compute I term, avoiding windup
    newpid->error_integral = constrain_abs(oldpid->error_integral + error, WINDUP_MAX);

    // Run PI controller
    float correction = error * KP + newpid->error_integral * KI;

    newpid->throttle = constrain(throttle+correction, 0, 1);
}

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    double joyvals[10] = {};

    _joystick->poll(joyvals);

    static alt_hold_pid_t _pid;

    // [-1,+1] => [0,1]
    float throttle = (joyvals[0] + 1) / 2;

    // NED => ENU
    float altitude   = -_dynamics->vstate.z;
    float climb_rate = -_dynamics->vstate.dz;

    alt_hold_pid_t newpid = {};

    alt_hold(throttle, altitude, climb_rate, &_pid, &newpid);

    values[0] = newpid.throttle;
    values[1] = newpid.throttle;
    values[2] = newpid.throttle;
    values[3] = newpid.throttle;

    memcpy(&_pid, &newpid, sizeof(alt_hold_pid_t));

}

void FRustFlightManager::tick(void)
{
}
