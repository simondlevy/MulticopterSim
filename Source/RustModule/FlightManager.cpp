#include "FlightManager.hpp"
#include "../MainModule/Utils.hpp"

#include <SDL.h>

static const char * LIBRARY_FILENAME = "hackflight.dll";

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    _dynamics = dynamics;

    void * library_handle = SDL_LoadObject(LIBRARY_FILENAME);

    _run_alt_hold =
        (alt_hold_fun_t)SDL_LoadFunction(library_handle, "rust_run_alt_hold");

    _joystick = new IJoystick();
}

FRustFlightManager::~FRustFlightManager()
{
}

float FRustFlightManager::scaleAxis(float value)
{
    return value * 670;
}

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    double joyvals[10] = {};

    _joystick->poll(joyvals);

    demands_t demands = {
        (joyvals[0] + 1) / 2, // throttle [-1,+1] => [0,1]
        scaleAxis(joyvals[1]),
        scaleAxis(joyvals[2]),
        scaleAxis(joyvals[3]) 
    };

    static alt_hold_t _pid;

    alt_hold_t newpid = _run_alt_hold(&demands, &_dynamics->vstate, &_pid);

    values[0] = newpid.throttle;
    values[1] = newpid.throttle;
    values[2] = newpid.throttle;
    values[3] = newpid.throttle;

    memcpy(&_pid, &newpid, sizeof(alt_hold_t));
}

void FRustFlightManager::tick(void)
{
}
