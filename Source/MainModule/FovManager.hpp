/*
 * Abstract, threaded class for managing camera field-of-view in MulticopterSim 
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedManager.hpp"

class FFovManager : public FThreadedManager {

    friend class Camera;

    private:

        float _fov = 0;

    protected:

        // Constructor, called once on main thread
        FFovManager() : FThreadedManager()
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

}; // Class FFovManager
