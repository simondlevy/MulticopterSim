/*
 * Null video-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "VideoManager.hpp"

class NullVideoManager : public VideoManager {

    protected:

        virtual void processImageBytes(uint8_t * bytes) override
        { 
            (void)bytes;
        }

    public:

        // Constructor, called once on main thread
        NullVideoManager( UTextureRenderTarget2D * cameraRenderTarget, uint8_t id) 
            : VideoManager(cameraRenderTarget)
        {
            (void)id;
        }

}; // Class NullVideoManager

FLIGHTMODULE_API VideoManager * createVideoManager(UTextureRenderTarget2D * renderTarget, uint8_t id)
{
    return new NullVideoManager(renderTarget, id);
}
