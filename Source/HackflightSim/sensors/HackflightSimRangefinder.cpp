/*
* HackflightSimRangefinder.cpp: Class implementation for Rangefinder class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "HackflightSimRangefinder.h"

HackflightSimRangefinder::HackflightSimRangefinder(APawn * pawn) : HackflightSimSensor(pawn)
{
}

void HackflightSimRangefinder::init(void)
{
	_groundAltitude = getAltitude();
}

bool HackflightSimRangefinder::distanceAvailable(float & distance)
{
	float altitude = getAltitude() - _groundAltitude;

	FVector euler = FMath::DegreesToRadians(_pawn->GetActorQuat().Euler());

	// Hypoteneuse = adjacent / cosine
	distance = altitude / (cos(euler.X) * cos(euler.Y));

	//_rangeNoise.addNoise(&distance);

	return true;
}

float HackflightSimRangefinder::getAltitude(void)
{
	return _pawn->GetActorLocation().Z / 100; // cm => m
}