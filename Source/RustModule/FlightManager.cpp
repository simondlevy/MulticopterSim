#include "FlightManager.hpp"
#include "../MainModule/Utils.hpp"

#include <SDL.h>

static const char * LIBRARY_FILENAME = "hackflight.dll";

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    _dynamics = dynamics;

    void * library_handle = SDL_LoadObject(LIBRARY_FILENAME);

    _run_hackflight = (run_hackflight_t) SDL_LoadFunction(library_handle, "c_run_hackflight");

    _joystick = new IJoystick();
}

FRustFlightManager::~FRustFlightManager()
{
}

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    double joyvals[10] = {};

    _joystick->poll(joyvals);

    joyvals[0] = (joyvals[0] + 1) / 2; // [-1,+1] => [0,1]

    demands_t demands = { joyvals[0], joyvals[1], joyvals[2], joyvals[3] };

    auto hackflight = _run_hackflight(&demands, &_dynamics->vstate, &_alt_hold);

    auto motors = hackflight.motors;

    memcpy(&_alt_hold, &hackflight.alt_hold, sizeof(alt_hold_t));

    values[0] = motors.m1;
    values[1] = motors.m2;
    values[2] = motors.m3;
    values[3] = motors.m4;
}

void FRustFlightManager::tick(void)
{
}
