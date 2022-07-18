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

    _vec2_init = (vec2_init_t) SDL_LoadFunction(library_handle, "vec2_init");
}

FRustFlightManager::~FRustFlightManager()
{
}

void FRustFlightManager::getMotors(double time, double* values)
{
}

void FRustFlightManager::tick(void)
{
    auto v2 = _vec2_init();
    debugline("v2: %.2f %.2f", v2.x, v2.y);
}
