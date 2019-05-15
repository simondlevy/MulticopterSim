/*
 * MulticopterSim support for acquisition of camera images and processing using OpenCV
 *
 * Adapted from https://answers.unrealengine.com/questions/193827/how-to-get-texture-pixels-using-utexturerendertarg.html
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#ifdef _USE_OPENCV

// XXX support OpenCV for Windows only
#ifdef _WIN32

#include "ImageGrabber.h"

#include "opencv2/imgproc/imgproc.hpp"

ImageGrabber::ImageGrabber(UTextureRenderTarget2D* visionTextureRenderTarget)
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

ImageGrabber::~ImageGrabber(void)
{
}

// Runs on main thread
void ImageGrabber::grabImage(void)
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

#endif
#endif
