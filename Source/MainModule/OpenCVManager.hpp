/*
 * Abstract video-management class for MulticopterSim using OpenCV
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "VideoManager.hpp"

#include <opencv2/core.hpp>
#include <opencv2/imgproc/imgproc.hpp>

class OpenCVManager : public VideoManager {

    private:

        // RBGA image created from render target on main thread
        cv::Mat _rbga_image;

        // RGB image sent to subclass for processing
        cv::Mat _image;

    protected:

        virtual void processImageBytes(uint8_t * bytes) override
        { 
            // Copy the RBGA pixels to the private image
            FMemory::Memcpy(_rbga_image.data, bytes, _rows*_cols*4);

            // Convert RGBA => RGB for public image
            cv::cvtColor(_rbga_image, _image, CV_RGBA2RGB);

            // Virtual method implemented in subclass
            processImage(_image);
        }

        // Override this method for your video application
        virtual void processImage(cv::Mat image) = 0;

    public:

        // Constructor, called once on main thread
        OpenCVManager( UTextureRenderTarget2D * cameraRenderTarget) : VideoManager(cameraRenderTarget)
    {
        // Create a private RBGA image for acquiring render target on main thread
        _rbga_image = cv::Mat::zeros(_rows, _cols, CV_8UC4);

        // Create a public OpenCV BGR image for uses by other classes
        _image = cv::Mat::zeros(_rows, _cols, CV_8UC3);

    }

}; // Class OpenCVManager
