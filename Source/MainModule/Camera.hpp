/*
 * Abstract camera class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Utils.hpp"

class Camera {

    friend class Vehicle;

    public:

        // Arbitrary array limits supporting statically declared assets
        static const uint8_t MAX_CAMERAS = 10; 

        // Supported resolutions
        typedef enum {

            RES_640x480,
            RES_1280x720,
            RES_1920x1080

        } Resolution_t;

    private:

        static constexpr float CAMERA_X = +20;
        static constexpr float CAMERA_Y =   0;
        static constexpr float CAMERA_Z = +30;

        // Byte array for RGBA image
        uint8_t * _imageBytes = NULL;

    protected:

        // Image size and field of view, set in constructor
        uint16_t _rows = 0;
        uint16_t _cols = 0;
        float    _fov  = 0;

        // UE4 resources, set in Vehicle::addCamera()
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
            _captureComponent = NULL;
            _renderTarget = NULL;
        }

        // Called by Vehicle::addCamera()
        void addToVehicle(APawn * pawn, USpringArmComponent * springArm, uint8_t id)
        {
            // Create name of render target asset
            static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>cameraTextureObject(L"/Game/Flying/RenderTargets/renderTarget_640x480_1");

            // Create a static render target.  This provides less flexibility than creating it dynamically,
            // but acquiring the pixels seems to run twice as fast.
            UTextureRenderTarget2D * textureRenderTarget2D = cameraTextureObject.Object;

            // Create a scene-capture component and set its target to the render target
            _captureComponent = pawn->CreateDefaultSubobject<USceneCaptureComponent2D >(makeName("Capture", id));
            _captureComponent->SetWorldScale3D(FVector(0.1,0.1,0.1));
            _captureComponent->SetupAttachment(springArm, USpringArmComponent::SocketName);
            _captureComponent->SetRelativeLocation(FVector(CAMERA_X, CAMERA_Y, CAMERA_Z));
            _captureComponent->TextureTarget = textureRenderTarget2D;

            // Get the render target resource for copying the image pixels
            _renderTarget = textureRenderTarget2D->GameThread_GetRenderTargetResource();
        }

        // Override this method for your video application
        virtual void processImageBytes(uint8_t * bytes) { (void)bytes; }

        // Sets current FOV
        void setFov(float fov)
        {
            _captureComponent->FOVAngle = _fov;
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

        virtual ~Camera()
        {
            delete _imageBytes;
        }

}; // Class Camera
