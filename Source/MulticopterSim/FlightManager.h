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

class FFlightManager : public FThreadedWorker {

    private:

        MultirotorDynamics * _dynamics;

        double * _motorvals; 

        double _previousTime;

        uint8_t _motorCount;

        virtual void update(double deltaT, double quat[4], double gyro[4], double * motorvals)  = 0;

    protected:

        virtual void performTask(void) override;

        FFlightManager(class AVehiclePawn * vehiclePawn, uint8_t motorCount, 
                double initialPosition[3], double initialRotation[3]);

    public:


        ~FFlightManager(void);

        void getPoseAndMotors(double deltaT, double position[3], double rotation[3], double * motorvals);

        static FFlightManager * createFlightManager(
                class AVehiclePawn * vehiclePawn, 
                double initialPosition[3], 
                double initialRotation[3]);
};
