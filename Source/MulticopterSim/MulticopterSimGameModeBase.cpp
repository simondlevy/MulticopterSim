// Fill out your copyright notice in the Description page of Project Settings.


#include "MulticopterSimGameModeBase.h"
#include "VehiclePawn.h"

AMulticopterSimGameModeBase::AMulticopterSimGameModeBase()
{
	// set default pawn class to our flying pawn
	DefaultPawnClass = AVehiclePawn::StaticClass();
}



