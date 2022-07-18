//#define WIN32_LEAN_AND_MEAN

//#define _USE_MATH_DEFINES
//#include <cmath>

#include "FlightManager.hpp"
#include "../MainModule/Utils.hpp"

#include <SDL.h>

static const char * LIBRARY_FILENAME = "hackflight.dll";

typedef float f32;

struct Vec2 {
    f32 x;
    f32 y;
};

typedef Vec2 (*vec2_init_t)();

static void * library_handle;

static vec2_init_t vec2_init;

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
    library_handle = SDL_LoadObject(LIBRARY_FILENAME);

    vec2_init = (vec2_init_t) SDL_LoadFunction(library_handle, "vec2_init");
}

FRustFlightManager::~FRustFlightManager()
{
}

void FRustFlightManager::getMotors(double time, double* values)
{
    //auto v2 = vec2_init();
}

void FRustFlightManager::tick(void)
{
    auto v2 = vec2_init();
    debugline("v2: %.2f %.2f", v2.x, v2.y);
}
