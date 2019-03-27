/*
 * ImageGrabber.h: MulticopterSim support for acquisition of camera images and processing using OpenCV
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

#ifdef _USE_OPENCV
#include <opencv2/core.hpp>

class ImageGrabber
{

private:

	// Access to camera's render target resource
	FRenderTarget * _renderTargetResource;

	// RBGA image create from render target on main thread
	cv::Mat _rbga_image;

public:

    // Available for use by other classes on all threads
	cv::Mat image;
	bool ready;

    // Constructor
	ImageGrabber(UTextureRenderTarget2D* visionTextureRenderTarget);

    // Destructor
	virtual ~ImageGrabber(void);

	// Should be called on the main thread
	void grabImage(void);
};
#endif
