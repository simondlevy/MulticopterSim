/*
 * ImageGrabber.cpp: MulticopterSim support for acquisition of camera images and processing by OpenCV
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "ImageGrabber.h"

ImageGrabber::ImageGrabber(UTextureRenderTarget2D* visionTextureRenderTarget)
{
	_visionTextureRenderTarget = visionTextureRenderTarget;

	// Creates Texture2D to store VisionTex content
	UTexture2D* visionTexture = UTexture2D::CreateTransient(_visionTextureRenderTarget->SizeX, _visionTextureRenderTarget->SizeY, PF_B8G8R8A8);

#if WITH_EDITORONLY_DATA
	visionTexture->MipGenSettings = TMGS_NoMipmaps;
#endif
	visionTexture->SRGB = visionTextureRenderTarget->SRGB;

	_visionRenderTarget = visionTextureRenderTarget->GameThread_GetRenderTargetResource();

	// Allocate memory for BGR image bytes
	_rows = _visionTextureRenderTarget->SizeY;
	_cols = _visionTextureRenderTarget->SizeX;
	_bgrbytes = new uint8_t[_rows*_cols * 3];

	// Create an empty OpenCV image
	_img= cv::Mat(_rows, _cols, CV_8UC3);

    // Make the images bytes be our BGR bytes
    _img.data = _bgrbytes;
}

ImageGrabber::~ImageGrabber(void)
{
	delete _bgrbytes;
}

void ImageGrabber::grabImage(void)
{
    // Read the pixels from the RenderTarget and store them in a FColor array
    _visionRenderTarget->ReadPixels(_visionSurfData);

    // Convert the FColor array to an RGB byte array

    for (int x = 0; x < _cols; ++x) {

        for (int y = 0; y < _rows; ++y) {

            int k = x + y * _cols;

            FColor PixelColor = _visionSurfData[k];

            _bgrbytes[k * 3] = PixelColor.B;
            _bgrbytes[k * 3 + 1] = PixelColor.G;
            _bgrbytes[k * 3 + 2] = PixelColor.R;
        }

    }

    // Process it in the subclass method
    processImage();
}

