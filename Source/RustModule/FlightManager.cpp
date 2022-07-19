//#define WIN32_LEAN_AND_MEAN

//#define _USE_MATH_DEFINES
//#include <cmath>

#include "FlightManager.hpp"
#include "../MainModule/Utils.hpp"

#include <SDL.h>

static const char * LIBRARY_FILENAME = "hackflight.dll";

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    void * library_handle = SDL_LoadObject(LIBRARY_FILENAME);

    _get_motors = (get_motors_t) SDL_LoadFunction(library_handle, "get_motors");
}

FRustFlightManager::~FRustFlightManager()
{
}

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    auto motors = _get_motors();

    values[0] = motors.m1;
    values[1] = motors.m2;
    values[2] = motors.m3;
    values[3] = motors.m4;
}

void FRustFlightManager::tick(void)
{
}
