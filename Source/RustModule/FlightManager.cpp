//#define WIN32_LEAN_AND_MEAN

//#define _USE_MATH_DEFINES
//#include <cmath>

#include "FlightManager.hpp"

#include <SDL.h>

// Data shared between FlightManager and Rust ------------------

static Dynamics * _dyn;

FRustFlightManager::FRustFlightManager(APawn * pawn, Dynamics * dynamics)
    : FFlightManager(dynamics)
{
}

FRustFlightManager::~FRustFlightManager()
{
}

void FRustFlightManager::getMotors(double time, double* values)
{
}

void FRustFlightManager::tick(void)
{
}
