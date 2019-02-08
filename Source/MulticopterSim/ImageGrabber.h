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
	virtual void processImage(void) = 0;

protected:

	cv::Mat _img;


private:

	// Support for vision algorithms
	int _rows;
	int _cols;
	uint8_t* _bgrabytes; // OpenCV uses BGRA

	// Access to camera's render target
	UTextureRenderTarget2D* _visionTextureRenderTarget;
	FRenderTarget* _visionRenderTarget;
	TArray<FColor> _visionSurfData;
	UTexture2D *Texture;
};
