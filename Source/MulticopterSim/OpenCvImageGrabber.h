/*
 * OpenCvImageGrabber.h: MulticopterSim support for acquisition of camera images and processing by OpenCV
 *
 * You should subclass this class, implementing the processImage() method
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

#include "ImageGrabber.h"

#include <opencv2/core.hpp>

/**
 * 
 */
class OpenCvImageGrabber : public ImageGrabber
{

public:

	OpenCvImageGrabber(UTextureRenderTarget2D* textureRenderTarget);

    // Called on its own thread (not main thread)
    void processImage(void) override;

    // Available for use by other classes
	cv::Mat image;

protected:

    // Called on the main thread
    virtual void copyImageData(void * srcData, int32_t count) override;
};
