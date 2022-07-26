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

void FRustFlightManager::getMotors(double time, double* values)
{
    (time);

    static bool _inBandPrev;
    static float _errorI;
    static float _altitudeTarget;
 
    double joyvals[10] = {};

    _joystick->poll(joyvals);

    // [-1,+1] => [0,1]
    float throttle = (joyvals[0] + 1) / 2;

    // NED => ENU
    float altitude = -_dynamics->vstate.z;

    debugline("%f", altitude);

    values[0] = 0.6;
    values[1] = 0.6;
    values[2] = 0.6;
    values[3] = 0.6;
}

void FRustFlightManager::tick(void)
{
}
