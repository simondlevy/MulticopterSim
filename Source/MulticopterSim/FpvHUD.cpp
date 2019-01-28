/*
* FpvHUD.cpp: Class implementation for Vision HUD in MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "FpvHUD.h"
#include "VehiclePawn.h"

AFpvHUD::AFpvHUD(void)
{
    // Done statically (at compile time), so must be repeated for each camera (render target)
	static ConstructorHelpers::FObjectFinder<UTextureRenderTarget2D>textureObject(TEXT("/Game/Flying/RenderTargets/FPVCameraTarget"));

    // Grab the texture render target for this camera
    _textureRenderTarget = textureObject.Object;
}

AFpvHUD::~AFpvHUD(void)
{
}

void AFpvHUD::DrawHUD(void)
{
	Super::DrawHUD();

	// Draw the image to the HUD
	DrawTextureSimple(_textureRenderTarget, LEFTX, TOPY, 1.0f, true);

	// Draw a border around the image
	drawBorder();
}

void AFpvHUD::drawBorder(float lxoff)
{
	float leftx   = LEFTX + lxoff;
	float rightx  = leftx + _textureRenderTarget->SizeX;
	float bottomy = TOPY + _textureRenderTarget->SizeY;

	drawBorderLine(leftx,  TOPY,    rightx, TOPY);
	drawBorderLine(rightx, TOPY,    rightx, bottomy);
	drawBorderLine(rightx, bottomy, leftx,  bottomy);
	drawBorderLine(leftx,  bottomy, leftx,  TOPY);
}

void AFpvHUD::drawBorderLine(float lx, float uy, float rx, float by)
{
	DrawLine(lx, uy, rx, by, BORDER_COLOR, BORDER_WIDTH);
}
