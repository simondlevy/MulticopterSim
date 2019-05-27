/*
* On-Screen Display for MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "Engine.h"
#include "Engine/World.h"

static void osd(char * buf, bool err)
{

    // Show message in yellow, error in red
    FColor TEXT_COLOR = err ? FColor::Red : FColor::Yellow;

    // Scale text to fit message
    float  TEXT_SCALE = 100.f / strlen(buf);

    // on screen
    if (GEngine) {

        // -1 = no overwrite (0 for overwrite); 5.f = arbitrary time to display; true = newer on top
        GEngine->AddOnScreenDebugMessage(0, 5.f, TEXT_COLOR, FString(buf), true, FVector2D(TEXT_SCALE,TEXT_SCALE));
    }
}
~
