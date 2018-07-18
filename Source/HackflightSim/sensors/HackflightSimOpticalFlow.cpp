/*
* HackflightSimOpticalFlow.cpp: Class implementation for OpticalFlow class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "HackflightSimOpticalFlow.h"

HackflightSimOpticalFlow::HackflightSimOpticalFlow(APawn * pawn)
{
	_pawn = pawn;
}

void HackflightSimOpticalFlow::getFlow(float flow[2])
{
	// Grab velocity and divide by 100 to get m/s
	FVector velocity = _pawn->GetVelocity() / 100;

	// Grab yaw angle
	float psi = FMath::DegreesToRadians(_pawn->GetActorQuat().Euler()).Z;

	// Use yaw angle to rotate inertial-frame X,Y velocities into body frame forward,rightward
	flow[0] = cos(psi)*velocity.X + sin(psi)*velocity.Y;
	flow[1] = cos(psi)*velocity.Y - sin(psi)*velocity.X;
}
