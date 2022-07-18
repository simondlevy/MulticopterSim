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
}

void FRustFlightManager::tick(void)
{
    auto motors = _get_motors();

    debugline("motors: %.2f %.2f %.2f %.2f",
            motors.m1, motors.m2, motors.m3, motors.m4);
}
