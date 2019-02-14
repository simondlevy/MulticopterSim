/*
 * ImageGrabber.cpp: MulticopterSim support for acquisition of camera images and processing by OpenCV
 *
 * Adapted from https://answers.unrealengine.com/questions/193827/how-to-get-texture-pixels-using-utexturerendertarg.html
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

// XXX support OpenCV for Windows only
#ifdef _WIN32

#include "OpenCvImageGrabber.h"

#include "opencv2/imgproc/imgproc.hpp"

OpenCvImageGrabber::OpenCvImageGrabber(UTextureRenderTarget2D* textureRenderTarget) : ImageGrabber(textureRenderTarget)
{
	uint16_t rows = textureRenderTarget->SizeY;
	uint16_t cols = textureRenderTarget->SizeX;

	// Create an empty OpenCV BGRA image
	image = cv::Mat(rows, cols, CV_8UC4);
}

// Runs on main thread
void OpenCvImageGrabber::copyImageData(void * srcData, int32_t count)
{
	FMemory::Memcpy(image.data, srcData, count);
}

// Runs on a separate thread
void OpenCvImageGrabber::processImage(void)
{
	// Convert from UE4 RGBA to OpenCV BGRA
	cv::cvtColor(image, image, CV_RGBA2BGRA);
}
#endif
