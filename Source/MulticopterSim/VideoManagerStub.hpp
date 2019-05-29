/*
 * Stubbed implementation of VideoManager class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"

class FVideoManager : public FThreadedWorker {

    protected:

        // Called once on main thread
        FVideoManager(UTextureRenderTarget2D * camera1RenderTarget, UTextureRenderTarget2D * camera2RenderTarget) 
            : FThreadedWorker()
        {
        }

        // Called repeatedly on worker thread to process current image
        void performTask(void)
        {
        }

    public:

        void grabCurrentImage(void)
        {
        }

        void useCamera1(void)
        {
        }

        void useCamera2(void)
        {
        }

        ~FVideoManager()
        {
        }

        // Factory method
        static FVideoManager * create(UTextureRenderTarget2D * camera1RenderTarget, UTextureRenderTarget2D * camera2RenderTarget)
        {
            return new FVideoManager(camera1RenderTarget, camera2RenderTarget);
        }

}; // Class FVideoManager
