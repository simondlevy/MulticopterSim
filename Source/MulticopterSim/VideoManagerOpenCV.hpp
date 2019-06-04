/*
 * Abstract, threaded video-management class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "ThreadedWorker.hpp"
#include "OpenCVImageGrabber.hpp"

class FVideoManager : public FThreadedWorker {

    private:

        // Image grabbers for our two cameras
        ImageGrabber * _camera1Grabber = NULL;
        ImageGrabber * _camera2Grabber = NULL;

        // Points to the current image grabber
        ImageGrabber * _currentCameraGrabber = NULL;

    protected:

        // Called once on main thread
        FVideoManager( UTextureRenderTarget2D * camera1RenderTarget, UTextureRenderTarget2D * camera2RenderTarget) 
            : FThreadedWorker()
        {
            // Create our two camera grabbers
            _camera1Grabber = new ImageGrabber(camera1RenderTarget);
            //_camera2Grabber = new ImageGrabber(camera2RenderTarget);

            // Start with camera 1
            _currentCameraGrabber = _camera1Grabber;
        }

        // Called repeatedly on worker thread to process current image
        void performTask(void)
        {
            if (_currentCameraGrabber->ready) {

                processImage(_currentCameraGrabber->image);
            }
        }

        // Override this method for your video application
        virtual void processImage(cv::Mat image) = 0;

    public:

        void grabCurrentImage(void)
        {
            _currentCameraGrabber->grabImage();
        }

        void useCamera1(void)
        {
            _currentCameraGrabber = _camera1Grabber;
        }

        void useCamera2(void)
        {
            _currentCameraGrabber = _camera2Grabber;
        }

        ~FVideoManager()
        {
            delete _camera1Grabber;
            delete _camera2Grabber;
        }

        // Factory method implemented by your subclass
        static FVideoManager * create(UTextureRenderTarget2D * camera1RenderTarget, UTextureRenderTarget2D * camera2RenderTarget);

}; // Class FVideoManager
