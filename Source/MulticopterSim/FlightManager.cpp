/*
 * ThreadedWorker subclass for dynamics computation
 *
 * This class contains very little code, because the dynamics computations are 
 * done in the platform-independent MultirotorDynamics class.
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "FlightManager.h"
#include "VehiclePawn.h"

// Called once on main thread
FFlightManager::FFlightManager(AVehiclePawn * vehiclePawn, class MultirotorDynamics * dynamics) : FThreadedWorker(vehiclePawn)
{
    _dynamics = dynamics;

    _previousTime = 0;
}

FFlightManager::~FFlightManager(void)
{
}

// Called repeatedly on worker thread
void FFlightManager::performTask(void)
{
    double currentTime = getCurrentTime();

    sprintf_s(_message, "%f", currentTime);

    if (_previousTime>0) {

        //_dynamics->update(currentTime-_previousTime);
    }

    _previousTime = currentTime;
}
