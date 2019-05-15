/*
 * Abstract, threaded flight-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.h"
#include "dynamics/MultirotorDynamics.h"

class FFlightManager : public FThreadedWorker
{
    private:

        MultirotorDynamics * _dynamics;

        double _previousTime;

    protected:

        virtual void performTask(void) override;

        FFlightManager(class AVehiclePawn * vehiclePawn, class MultirotorDynamics * dynamics);

    public:


        ~FFlightManager(void);

        virtual TArray<float> update(float deltaTime, FQuat quat, FVector gyro)  = 0;

        /**
         *  Factory method.
         */
        static FFlightManager * createFlightManager(class AVehiclePawn * vehiclePawn, class MultirotorDynamics * dynamics);
};
