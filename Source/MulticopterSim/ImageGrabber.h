/*
 * ImageGrabber.h: MulticopterSim support for acquisition of camera images
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

/**
 * 
 */
class MULTICOPTERSIM_API ImageGrabber
{

public:

	ImageGrabber(UTextureRenderTarget2D* textureRenderTarget);

	// Called on the main thread
	void grabImage(void);

protected:

    virtual void copyImageData(void * srcData, int32_t count) { (void)srcData; (void)count; }

private:

	// Access to camera's render target
	FRenderTarget* _renderTarget;
};
