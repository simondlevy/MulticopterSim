// Copyright 1998-2017 Epic Games, Inc. All Rights Reserved.

#include "HackflightSimGameMode.h"
#include "HackflightSimPawn.h"

AHackflightSimGameMode::AHackflightSimGameMode()
{
	// set default pawn class to our flying pawn
	DefaultPawnClass = AHackflightSimPawn::StaticClass();
}
