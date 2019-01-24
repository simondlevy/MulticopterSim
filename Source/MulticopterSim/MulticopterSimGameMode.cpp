// Copyright 1998-2017 Epic Games, Inc. All Rights Reserved.

#include "MulticopterSimGameMode.h"
#include "VehiclePawn.h"

AMulticopterSimGameMode::AMulticopterSimGameMode()
{
	// set default pawn class to our flying pawn
	DefaultPawnClass = AVehiclePawn::StaticClass();
}
