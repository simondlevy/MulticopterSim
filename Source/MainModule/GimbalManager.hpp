/*
 * Abstract, threaded gimbal-management class for MulticopterSim 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"

class FGimbalManager : public FThreadedWorker {

    private:

        static constexpr float FOV_INIT = 70;

        float _roll  = 0;
        float _pitch = 0;
        float _yaw   = 0;

    protected:

        // Constructor, called once on main thread
        FGimbalManager() : FThreadedWorker()
        {
            _roll  = 0;
            _pitch = 0;
            _yaw   = 0;
        }

        // Called repeatedly on worker thread to process current image
        void performTask(double currentTime)
        {
            get(currentTime, _roll, _pitch, _yaw);
        }

        virtual void get(double currentTime, float & roll, float & pitch, float & yaw) = 0;

    public:

        // Called on main thread
        void get(float & roll, float & pitch, float & yaw) 
        {
            roll = _roll;
            pitch = _pitch;
            yaw = _yaw;
        }

        ~FGimbalManager()
        {
        }

}; // Class FGimbalManager
