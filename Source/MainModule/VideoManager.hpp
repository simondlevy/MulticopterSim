/*
 * Abstract video-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "CoreMinimal.h"
#include "Engine.h"
#include "GameFramework/HUD.h"
#include "GameFramework/HUD.h"
#include "Engine/TextureRenderTarget2D.h"
#include "Debug.hpp"


class VideoManager {

    private:

        // Access to camera's render target resource
        FRenderTarget * _renderTargetResource = NULL;

        // Byte array for RGBA image
        uint8_t * _imageBytes = NULL;

    protected:

        // Image size
        uint16_t _rows = 0;
        uint16_t _cols = 0;

        // Constructor, called once on main thread
        VideoManager( UTextureRenderTarget2D * cameraRenderTarget) 
        {
            // compute the size of the image
            _rows = cameraRenderTarget->SizeY;
            _cols = cameraRenderTarget->SizeX;

            // Create a byte array sufficient to hold the RGBA image
            _imageBytes = new uint8_t [_rows*_cols*4];

            // Get the render target resource for copying the image pixels
            _renderTargetResource = cameraRenderTarget->GameThread_GetRenderTargetResource();
        }

        // Override this method for your video application
        virtual void processImageBytes(uint8_t * bytes) { (void)bytes; }

    public:

        // Called on main thread
        void grabImage(void)
        {
            // Read the pixels from the RenderTarget
            TArray<FColor> renderTargetPixels;
            _renderTargetResource->ReadPixels(renderTargetPixels);

            // Copy the RBGA pixels to the private image
            FMemory::Memcpy(_imageBytes, renderTargetPixels.GetData(), _rows*_cols*4);

            // Virtual method implemented in subclass
            processImageBytes(_imageBytes);
        }

        ~VideoManager()
        {
            delete _imageBytes;
        }

}; // Class VideoManager
