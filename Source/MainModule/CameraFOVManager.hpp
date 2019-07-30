/*
 * Abstract, threaded class for camera FOV in MulticopterSim 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"

class FCameraFOVManager : public FThreadedWorker {

    friend class Camera;

    private:

    protected:

        // Constructor, called once on main thread
        FCameraFOVManager() : FThreadedWorker()
        {
        }

        // Called repeatedly on worker thread to process current image
        void performTask(double currentTime)
        {
        }

        float getFOV(void)
        {
        }

}; // Class FCameraFOVManager
