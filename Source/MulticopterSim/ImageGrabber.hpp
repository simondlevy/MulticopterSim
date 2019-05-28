/*
 * MulticopterSim support for acquisition of camera images and processing using OpenCV
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

#include <opencv2/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>

class ImageGrabber
{

    private:

        // Access to camera's render target resource
        FRenderTarget * _renderTargetResource = NULL;

        // RBGA image create from render target on main thread
        cv::Mat _rbga_image = NULL;

    public:

        // Available for use by other classes on all threads
        cv::Mat image;
        bool ready;

        // Constructor
        ImageGrabber(UTextureRenderTarget2D* visionTextureRenderTarget)
        {
            // Get the size of the render target
            uint16_t rows = visionTextureRenderTarget->SizeY;
            uint16_t cols = visionTextureRenderTarget->SizeX;

            // Create a private RBGA image for acquiring render target on main thread
            _rbga_image = cv::Mat::zeros(rows, cols, CV_8UC4);

            // Create a public OpenCV BGR image for uses by other classes
            image = cv::Mat::zeros(rows, cols, CV_8UC3);

            // Get the render target resource for copying the image pixels
            _renderTargetResource = visionTextureRenderTarget->GameThread_GetRenderTargetResource();

            ready = false;
        }


        // Should be called on the main thread
        void grabImage(void)
        {
            // Read the pixels from the RenderTarget
            TArray<FColor> renderTargetPixels;
            _renderTargetResource->ReadPixels(renderTargetPixels);

            // Copy the RBGA pixels to the private image
            FMemory::Memcpy(_rbga_image.data, renderTargetPixels.GetData(), renderTargetPixels.Num() * 4);

            // Convert RGBA => RGB for public image
            cv::cvtColor(_rbga_image, image, CV_RGBA2RGB);

            ready = true;
        }

}; // class ImageGrabber
