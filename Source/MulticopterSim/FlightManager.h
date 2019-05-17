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

        // Constants specified/computed in constructor
        uint8_t _motorCount;
        double  _deltaT;

        // Dynamics
        double * _motorvals; 
        double   _position[3];
        double   _rotation[3];
        double   _previousTime;

        // Implement for each subclass
        virtual void update(double deltaT, double quat[4], double gyro[4], double * motorvals)  = 0;

    protected:

        class AVehiclePawn * _vehiclePawn;

        MultirotorDynamics * _dynamics;

        virtual void performTask(void) override;

        FFlightManager(
                class AVehiclePawn * vehiclePawn, 
                uint8_t   motorCount, 
                double    initialPosition[3], 
                double    initialRotation[3],
                uint16_t  updateFrequency=1000);

    public:

        ~FFlightManager(void);

        // Copies current pose and motor speeds for kinematics
        void getKinematics(double position[3], double rotation[3], double * motorvals);

        // Factory method implemented by your subclass
        static FFlightManager * createFlightManager(
                class AVehiclePawn * vehiclePawn, 
                double initialPosition[3], 
                double initialRotation[3]);
};
