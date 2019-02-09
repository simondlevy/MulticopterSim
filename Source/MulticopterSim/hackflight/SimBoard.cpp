/*
* SimBoard.cpp : ::Board class implementation for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "SimBoard.h"
#include "BuiltinPhysics.h"

#include <hackflight.hpp>

SimBoard::SimBoard(void)
{
}

SimBoard::~SimBoard(void)
{
}

void SimBoard::update(void)
{
}

// Factory method for SimBoard class
SimBoard * SimBoard::createSimBoard()
{
    return new SimBoard();
}

// Debugging
void hf::Board::outbuf(char * buf)
{
    AVehiclePawn::outbuf(buf);
}

// Factory method for Physics class
Physics * Physics::createPhysics(void)
{
    return new BuiltinPhysics();
}

