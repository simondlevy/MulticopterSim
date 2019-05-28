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

        UTextureRenderTarget2D * _camera1RenderTarget;
        UTextureRenderTarget2D * _camera2RenderTarget;

    protected:

        // Called once on main thread
        FVideoManager(UTextureRenderTarget2D * camera1RenderTarget, UTextureRenderTarget2D * camera2RenderTarget) : FThreadedWorker()
        {
            _camera1RenderTarget = camera1RenderTarget;
            _camera2RenderTarget = camera2RenderTarget;
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
        static FVideoManager * create(UTextureRenderTarget2D * camera1RenderTarget, UTextureRenderTarget2D * camera2RenderTarget);

}; // Class FVideoManager
