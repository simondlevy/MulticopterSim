/*
 * Abstract camera class for MulticopterSim
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


class Camera {

    public:

        typedef enum {

            RES_640x480,
            RES_1280x720,
            RES_1920x1080

        } Resolution_t;

    private:

        // Access to camera's render target resource
        FRenderTarget * _renderTargetResource = NULL;

        // Byte array for RGBA image
        uint8_t * _imageBytes = NULL;

    protected:

        // Image size
        uint16_t _rows = 0;
        uint16_t _cols = 0;

        // Override this method for your video application
        virtual void processImageBytes(uint8_t * bytes) { (void)bytes; }

    public:

        // Associates this video manager with a render target
        virtual void setRenderTarget(UTextureRenderTarget2D * renderTarget) 
        {
            // compute the size of the image
            _rows = renderTarget->SizeY;
            _cols = renderTarget->SizeX;

            // Create a byte array sufficient to hold the RGBA image
            _imageBytes = new uint8_t [_rows*_cols*4];

            // Get the render target resource for copying the image pixels
            _renderTargetResource = renderTarget->GameThread_GetRenderTargetResource();
        }

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

        virtual ~Camera()
        {
            delete _imageBytes;
        }

}; // Class Camera
