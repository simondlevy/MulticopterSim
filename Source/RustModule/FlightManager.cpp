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

static float alt_hold(float throttle, float altitude, float climb_rate)
{
    static constexpr float KP = 0.75;
    static constexpr float KI = 1.5;
    static constexpr float ALTITUDE_MIN   = 1.0;
    static constexpr float PILOT_VELZ_MAX = 2.5;
    static constexpr float STICK_DEADBAND = 0.2;
    static constexpr float WINDUP_MAX     = 0.4;

    static bool _inBandPrev;
    static float _errorI;
    static float _altitudeTarget;

    // Rescale throttle [0,1] => [-1,+1]
    float sthrottle = 2 * throttle - 1; 

    // Is stick demand in deadband, above a minimum altitude?
    bool inBand = fabs(sthrottle) < STICK_DEADBAND && altitude > ALTITUDE_MIN; 

    // Zero throttle will reset error integral
    bool atZeroThrottle = throttle == 0;

    // Reset controller when moving into deadband above a minimum altitude
    bool gotNewTarget = inBand && !_inBandPrev;
    _errorI = gotNewTarget || atZeroThrottle ? 0 : _errorI;

    if (atZeroThrottle) {
        _altitudeTarget = 0;
    }

    _altitudeTarget = gotNewTarget ? altitude : _altitudeTarget;

    // Target velocity is a setpoint inside deadband, scaled
    // constant outside
    float targetVelocity = inBand ?
        _altitudeTarget - altitude :
        PILOT_VELZ_MAX * sthrottle;

    // Compute error as scaled target minus actual
    float error = targetVelocity - climb_rate;

    // Compute I term, avoiding windup
    _errorI = constrain_abs(_errorI + error, WINDUP_MAX);

    // Run PI controller
    float correction = error * KP + _errorI * KI;

    return constrain(throttle+correction, 0, 1);
}

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    double joyvals[10] = {};

    _joystick->poll(joyvals);

    // [-1,+1] => [0,1]
    float throttle = (joyvals[0] + 1) / 2;

    // NED => ENU
    float altitude   = -_dynamics->vstate.z;
    float climb_rate = -_dynamics->vstate.dz;

    float new_throttle = alt_hold(throttle, altitude, climb_rate);

    values[0] = new_throttle;
    values[1] = new_throttle;
    values[2] = new_throttle;
    values[3] = new_throttle;
}

void FRustFlightManager::tick(void)
{
}
