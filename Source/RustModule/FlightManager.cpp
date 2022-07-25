#include "FlightManager.hpp"
#include "../MainModule/Utils.hpp"
#include "../MainModule/Joystick.h"

#include <SDL.h>

static const char * LIBRARY_FILENAME = "hackflight.dll";

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    _dynamics = dynamics;

    void * library_handle = SDL_LoadObject(LIBRARY_FILENAME);

    _get_motors = (get_motors_t) SDL_LoadFunction(library_handle, "get_motors");
}

FRustFlightManager::~FRustFlightManager()
{
}

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    static IJoystick * _joystick;

    if (!_joystick) {
        _joystick = new IJoystick();
    }

    double joyvals[10] = {};

    _joystick->poll(joyvals);

    joyvals[0] = (joyvals[0] + 1) / 2; // [-1,+1] => [0,1]

    demands_t demands = { joyvals[0], joyvals[1], joyvals[2], joyvals[3] };

    auto motors = _get_motors(&_dynamics->vstate);

    values[0] = motors.m1;
    values[1] = motors.m2;
    values[2] = motors.m3;
    values[3] = motors.m4;
}

void FRustFlightManager::tick(void)
{
}
