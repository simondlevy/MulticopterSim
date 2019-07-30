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

#include "CameraFovManager.hpp"

class Camera {

    friend class Vehicle;

    public:

        // Supported resolutions
        typedef enum {

            RES_640x480,
            RES_1280x720,
            RES_1920x1080

        } Resolution_t;

    private:

        // Byte array for RGBA image
        uint8_t * _imageBytes = NULL;

        // Optional threaded worker for getting FOV
        FCameraFovManager * _fovManager = NULL;

    protected:

        // Image size and field of view, set in constructor
        uint16_t _rows = 0;
        uint16_t _cols = 0;
        float    _fov  = 0;

        // UE4 resources, set in Vehicle::addCamera()
        UCameraComponent         * _cameraComponent = NULL;
        USceneCaptureComponent2D * _captureComponent = NULL;
        FRenderTarget            * _renderTarget = NULL;
 
        Camera(float fov, Resolution_t resolution) 
        {
            uint16_t rowss[3] = {480, 720, 1080};
            uint16_t colss[3] = {640, 1280, 1920};

            _rows = rowss[resolution];
            _cols = colss[resolution];
            _fov = fov;

            // Create a byte array sufficient to hold the RGBA image
            _imageBytes = new uint8_t [_rows*_cols*4];

            // These will be set in Vehicle::addCamera()
            _cameraComponent = NULL;
            _captureComponent = NULL;
            _renderTarget = NULL;

            // Optionally set by setFovManager()
            _fovManager = NULL;
        }

        // Override this method for your video application
        virtual void processImageBytes(uint8_t * bytes) { (void)bytes; }

        // Can be called by Camera and Vehicle classes
        void setFov(void)
        {
            _cameraComponent->SetFieldOfView(_fov);
            _captureComponent->FOVAngle = _fov - 45;
        }

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

        void setFovManager(FCameraFovManager * fovManager) 
        {
            _fovManager = fovManager;
        }

        virtual ~Camera()
        {
            delete _imageBytes;
        }

}; // Class Camera
