/*
* HackflightSimOpticalFlow.cpp: Class implementation for OpticalFlow class in HackflightSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "HackflightSimOpticalFlow.h"

#include <debug.hpp>

HackflightSimOpticalFlow::HackflightSimOpticalFlow(APawn * pawn) : HackflightSimSensor(pawn)
{
}

void HackflightSimOpticalFlow::modifyState(state_t & state, float time)
{
	(void)time;

	// Grab velocity and divide by 100 to get m/s
	FVector velocity = _pawn->GetVelocity() / 100;

	// Grab yaw angle
	float psi = getEulerAngles().Z;

	// Use yaw angle to rotate inertial-frame X,Y velocities into body frame forward,rightward
	state.velocityForward   = cos(psi)*velocity.X + sin(psi)*velocity.Y;
	state.velocityRightward = cos(psi)*velocity.Y - sin(psi)*velocity.X;

    // Integrate velocity to get position
    state.positionX += state.velocityRightward;
    state.positionY += state.velocityForward;
}

bool HackflightSimOpticalFlow::ready(float time)
{
    (void)time;
    return true;
}
