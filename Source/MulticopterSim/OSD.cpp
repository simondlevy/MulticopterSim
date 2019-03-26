/*
* OSD.cpp: On-Screen Display for MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "OSD.h"
#include "Engine.h"
#include "Engine/World.h"

void osd(char * buf)
{
    // Text properties for debugging
    FColor TEXT_COLOR = FColor::Yellow;
    constexpr float  TEXT_SCALE = 2.f;

    // on screen
    if (GEngine) {

        // -1 = no overwrite (0 for overwrite); 5.f = arbitrary time to display; true = newer on top
        GEngine->AddOnScreenDebugMessage(0, 5.f, TEXT_COLOR, FString(buf), true, FVector2D(TEXT_SCALE,TEXT_SCALE));
    }
}
