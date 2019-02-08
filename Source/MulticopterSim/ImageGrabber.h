/*
 * ImageGrabber.h: MulticopterSim support for acquisition of camera images and processing by OpenCV
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

#include <opencv2/core.hpp>

/**
 * 
 */
class MULTICOPTERSIM_API ImageGrabber
{

public:

	ImageGrabber(UTextureRenderTarget2D* visionTextureRenderTarget);

	~ImageGrabber(void);

	// Called on the main thread
	void grabImage(void);

	// Called on the other thread
	void processImage(void);

protected:

	cv::Mat _img;

private:

	// Access to camera's render target
	FRenderTarget* _renderTarget;
};
