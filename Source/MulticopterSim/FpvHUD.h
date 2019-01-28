/*
* FpvHUD.h: Class declaration for FPV HUD in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "CoreMinimal.h"
#include "Engine.h"
#include "GameFramework/HUD.h"
#include "GameFramework/HUD.h"
#include "Engine/TextureRenderTarget2D.h"

// For doing image processing within FpvHUD
//#include "myproject/MyImageGrabber.h"

#include "FpvHUD.generated.h"

/**
 * 
 */
UCLASS()
class AFpvHUD : public AHUD
{
	GENERATED_BODY()
	
	AFpvHUD();

	~AFpvHUD();

	virtual void DrawHUD(void) override;

	const float LEFTX  = 20;
	const float TOPY   = 40;

	const float BORDER_WIDTH = 2.0f;
	
	const FLinearColor BORDER_COLOR = FLinearColor::Yellow;

	void drawBorderLine(float lx, float uy, float rx, float by);

	void drawBorder(float lxoff = 0);

	UTextureRenderTarget2D* _textureRenderTarget;

	// You can do image processing with this
    //MyImageGrabber * _imageGrabber;
};
