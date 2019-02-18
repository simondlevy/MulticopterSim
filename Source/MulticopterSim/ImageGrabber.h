/*
 * ImageGrabber.h: MulticopterSim support for acquisition of camera images and processing
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

class ImageGrabber
{

public:

	ImageGrabber(UTextureRenderTarget2D* visionTextureRenderTarget);

	virtual ~ImageGrabber(void);

	// Called on the main thread
	void grabImage(void);

	// Called on the other thread
	virtual void processImage(void) { }

protected:

    virtual void copyImageData(void * srcData, int32_t count) { }

private:

	// Access to camera's render target
	FRenderTarget* _renderTarget;
};
