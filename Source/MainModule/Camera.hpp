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

    friend class Vehicle;

    public:

        typedef enum {

            RES_640x480,
            RES_1280x720,
            RES_1920x1080

        } Resolution_t;

    protected:

        // Byte array for RGBA image
        uint8_t * _imageBytes = NULL;

        // Image size
        uint16_t _rows = 0;
        uint16_t _cols = 0;
        
        // Field of view
        float _fov = 0;

        Resolution_t _resolution;

        // UE4 resources
        UCameraComponent         * _cameraComponent = NULL;
        USceneCaptureComponent2D * _captureComponent = NULL;
        FRenderTarget            * _renderTarget = NULL;
 
        Camera(float fov, Resolution_t resolution) 
        {
            _fov = fov;
            _resolution = resolution;

            // These will be set in Vehicle::addCamera()
            _cameraComponent = NULL;
            _captureComponent = NULL;
            _renderTarget = NULL;
        }

        // Override this method for your video application
        virtual void processImageBytes(uint8_t * bytes) { (void)bytes; }

    public:

        // Called on main thread
        void grabImage(void)
        {
            // Read the pixels from the RenderTarget
            TArray<FColor> renderTargetPixels;
            _renderTarget->ReadPixels(renderTargetPixels);

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
