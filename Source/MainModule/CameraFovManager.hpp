/*
 * Abstract, threaded class for managing camera field-of-view in MulticopterSim 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"

class FCameraFovManager : public FThreadedWorker {

    friend class Camera;

    private:

        float _fov = 0;

    protected:

        // Constructor, called once on main thread
        FCameraFovManager() : FThreadedWorker()
        {
            _fov = 0;
        }

        // Called repeatedly on worker thread to process current image
        void performTask(double currentTime)
        {
        }

        float getFov(void)
        {
            return _fov;
        }

}; // Class FCameraFovManager
