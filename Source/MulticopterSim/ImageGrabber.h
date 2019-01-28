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

	void grabImage(void);
    
protected:

	cv::Mat _img;

    virtual void processImage(void) = 0;

private:

	// Access to camera's render target
	UTextureRenderTarget2D* _visionTextureRenderTarget;
	FRenderTarget* _visionRenderTarget;
	TArray<FColor> _visionSurfData;

	// Support for vision algorithms
	int _rows;
	int _cols;
	uint8_t* _bgrbytes; // OpenCV uses BGR o
};
