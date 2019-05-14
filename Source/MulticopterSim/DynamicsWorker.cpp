/*
 * DynamicsWorker.cpp: ThreadedWorker subclass for dynamics computation
 *
 * This class contains very little code, because the dynamics computations are 
 * done in the platform-independent MultirotorDynamics class.
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "DynamicsWorker.h"
#include "VehiclePawn.h"

// Called once on main thread
FDynamicsWorker::FDynamicsWorker(AVehiclePawn * vehiclePawn, class MultirotorDynamics * dynamics) : FThreadedWorker(vehiclePawn)
{
    _dynamics = dynamics;

    _previousTime = 0;
}

FDynamicsWorker::~FDynamicsWorker(void)
{
}

// Called repeatedly on worker thread
void FDynamicsWorker::performTask(void)
{
    double currentTime = getCurrentTime();

    if (_previousTime>0) {

        //_dynamics->update(currentTime-_previousTime);
    }

    _previousTime = currentTime;
}
