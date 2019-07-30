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

    protected:

        // Constructor, called once on main thread
        FCameraFovManager() : FThreadedWorker()
        {
        }

        // Called repeatedly on worker thread to process current image
        void performTask(double currentTime)
        {
        }

}; // Class FCameraFovManager
