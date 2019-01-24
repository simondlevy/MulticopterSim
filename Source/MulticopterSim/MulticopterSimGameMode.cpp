// Copyright 1998-2017 Epic Games, Inc. All Rights Reserved.

#include "MulticopterSimGameMode.h"
#include "HackflightSimPawn.h"

AMulticopterSimGameMode::AMulticopterSimGameMode()
{
	// set default pawn class to our flying pawn
	DefaultPawnClass = AHackflightSimPawn::StaticClass();
}
