/*
 * ImageGrabber.cpp: MulticopterSim support for acquisition of camera images and processing by OpenCV
 *
 * Adapted from https://answers.unrealengine.com/questions/193827/how-to-get-texture-pixels-using-utexturerendertarg.html
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "ImageGrabber.h"
#include "opencv2/highgui/highgui.hpp"
#include "opencv2/imgproc/imgproc.hpp"


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
	_bgrabytes = new uint8_t[_rows*_cols * 4];

	// Create an empty OpenCV image
	_img= cv::Mat(_rows, _cols, CV_8UC4);

    // Make the images bytes be our BGR bytes
    _img.data = _bgrabytes;
}

ImageGrabber::~ImageGrabber(void)
{
	delete _bgrabytes;
}

void ImageGrabber::grabImage(void)
{
	// Creates Texture2D to store TextureRenderTarget content
	//Texture = UTexture2D::CreateTransient(_visionTextureRenderTarget->SizeX, _visionTextureRenderTarget->SizeY, PF_B8G8R8A8);
	Texture = UTexture2D::CreateTransient(_visionTextureRenderTarget->SizeX, _visionTextureRenderTarget->SizeY, PF_R8G8B8A8);
#if WITH_EDITORONLY_DATA
	Texture->MipGenSettings = TMGS_NoMipmaps;
#endif
	Texture->SRGB = _visionTextureRenderTarget->SRGB;

	// Read the pixels from the RenderTarget and store them in a FColor array
	TArray<FColor> SurfData;
	FRenderTarget *RenderTarget = _visionTextureRenderTarget->GameThread_GetRenderTargetResource();
	RenderTarget->ReadPixels(SurfData);

	// Lock and copies the data between the textures
	//void* TextureData = Texture->PlatformData->Mips[0].BulkData.Lock(LOCK_READ_WRITE);
	//const int32 TextureDataSize = SurfData.Num() * 4;
	//FMemory::Memcpy(TextureData, SurfData.GetData(), TextureDataSize);
	//Texture->PlatformData->Mips[0].BulkData.Unlock();

	FMemory::Memcpy(_bgrabytes, SurfData.GetData(), SurfData.Num() * 4);

	cv::cvtColor(_img, _img, CV_RGBA2BGRA);

    // Read the pixels from the RenderTarget and store them in a FColor array
    //_visionRenderTarget->ReadPixels(_visionSurfData);
}

