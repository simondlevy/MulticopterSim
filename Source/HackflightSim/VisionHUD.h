/*
* VisionHud.h: Class declaration for Vision HUD in HackflightSim
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

#include "VisionHUD.generated.h"

/**
 * 
 */
UCLASS()
class HACKFLIGHTSIM_API AVisionHUD : public AHUD
{
	GENERATED_BODY()
	
	AVisionHUD();

	virtual void DrawHUD() override;

	const float LEFTX  = 20;
	const float TOPY   = 40;
	const float WIDTH  = 256;
	const float HEIGHT = 128;
	
	const FLinearColor BORDER_COLOR = FLinearColor::Yellow;
	const float BORDER_WIDTH = 2.0f;

	void drawBorder(float lx, float uy, float rx, float by);

	// Access to Vision camera
	UTextureRenderTarget2D* VisionTextureRenderTarget;
	FRenderTarget* VisionRenderTarget;
	TArray<FColor> VisionSurfData;

	// Support for vision algorithms
	int rows;
	int cols;
	uint8_t* imagergb;
};
