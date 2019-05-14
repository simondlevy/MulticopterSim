/*
 * DynamicsWorker.h: ThreadedWorker subclass for dynamics computation
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.h"
#include "dynamics/MultirotorDynamics.h"

class FDynamicsWorker :	public FThreadedWorker {

    private:

        MultirotorDynamics * _dynamics;

        double _previousTime;


    protected:

        virtual void performTask(void) override;

    public:

        FDynamicsWorker(class AVehiclePawn * vehiclePawn, class MultirotorDynamics * dynamics);

        ~FDynamicsWorker(void);
};
