/*
 * Abstract, threaded video-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"

class FVideoManager : public FThreadedWorker {

    private:


    protected:

        // Called once on main thread
        FVideoManager(uint8_t motorCount, FVector initialLocation, FRotator initialRotation, uint16_t updateFrequency=1000) : FThreadedWorker()
        {
        }
        
        // Called repeatedly on worker thread to compute dynamics and run flight controller (PID)
        void performTask(void)
        {
        }

    public:

        ~FVideoManager(void)
        {
        }

        // Factory method implemented by your subclass
        static FVideoManager * createVideoManager(FVector initialLocation, FRotator initialRotation);
};
