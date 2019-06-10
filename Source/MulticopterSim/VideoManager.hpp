/*
 * Abstract, threaded video-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"

#include "CoreMinimal.h"
#include "Engine.h"
#include "GameFramework/HUD.h"
#include "GameFramework/HUD.h"
#include "Engine/TextureRenderTarget2D.h"

#include <opencv2/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>

class FVideoManager : public FThreadedWorker {

    private:

        // Access to camera's render target resource
        FRenderTarget * _renderTargetResource = NULL;

        // RBGA image created from render target on main thread
        cv::Mat _rbga_image;

        // RGB image sent to subclass for processing
        cv::Mat _image;

        // Helps avoid grabbing image before one is available
        bool _ready = false;

    protected:

        // Constructor, called once on main thread
        FVideoManager( UTextureRenderTarget2D * cameraRenderTarget) 
            : FThreadedWorker()
        {
            // Get the size of the render target
            uint16_t rows = cameraRenderTarget->SizeY;
            uint16_t cols = cameraRenderTarget->SizeX;

            // Create a private RBGA image for acquiring render target on main thread
            _rbga_image = cv::Mat::zeros(rows, cols, CV_8UC4);

            // Create a public OpenCV BGR image for uses by other classes
            _image = cv::Mat::zeros(rows, cols, CV_8UC3);

            // Get the render target resource for copying the image pixels
            _renderTargetResource = cameraRenderTarget->GameThread_GetRenderTargetResource();

            // No image yet
            _ready = false;
        }

        // Called repeatedly on worker thread to process current image
        void performTask(void)
        {
            if (_ready) {

                processImage(_image);
            }
        }

        // Override this method for your video application
        virtual void processImage(cv::Mat image) = 0;

    public:

        // Called on main thread
        void grabImage(void)
        {
            // Read the pixels from the RenderTarget
            TArray<FColor> renderTargetPixels;
            _renderTargetResource->ReadPixels(renderTargetPixels);

            // Copy the RBGA pixels to the private image
            FMemory::Memcpy(_rbga_image.data, renderTargetPixels.GetData(), renderTargetPixels.Num() * 4);

            // Convert RGBA => RGB for public image
            cv::cvtColor(_rbga_image, _image, CV_RGBA2RGB);

            _ready = true;
        }

        ~FVideoManager()
        {
        }

        // Factory method implemented by your subclass
        static FVideoManager * create(UTextureRenderTarget2D * cameraRenderTarget);

}; // Class FVideoManager
