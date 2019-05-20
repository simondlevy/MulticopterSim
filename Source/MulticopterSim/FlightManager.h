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

    friend class AVehiclePawn;

    private:

        // Constants specified/computed in constructor
        uint8_t _motorCount;
        double  _deltaT;

        // Start-time offset so timing begins at zero
        double _startTime;

        // Kinematics
        double   _position[3];
        double   _rotation[3];
        double * _motorvals; 
        
        // For computing _deltaT
        double   _previousTime;

        // Useful conversion function
        static void eulerToQuaternion(double eulerAngles[3], double quaternion[4]);
        // Implement for each subclass
        virtual void update(double time, double quat[4], double gyro[4], double * motorvals)  = 0;

    protected:

        MultirotorDynamics * _dynamics;

        virtual void performTask(void) override;

        virtual void getGimbal(float & roll, float &pitch) { roll = 0; pitch = 0; }

        FFlightManager(
                uint8_t   motorCount, 
                double    initialPosition[3], 
                double    initialRotation[3],
                uint16_t  updateFrequency=1000);

    public:

        ~FFlightManager(void);

        // Copies current pose and motor speeds for kinematics
        void getKinematics(double position[3], double rotation[3], double * motorvals);

        // Factory method implemented by your subclass
        static FFlightManager * createFlightManager(double initialPosition[3], double initialRotation[3]);
};
