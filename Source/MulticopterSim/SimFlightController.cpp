/*
* SimFlightController.cpp: Abstract flight-control class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#include "SimFlightController.h"

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include <hackflight.hpp>
#include "SimReceiver.h"

// Main firmware
static hf::Hackflight hackflight;

// Receiver (joystick)
static hf::SimReceiver * receiver;

class SimBoard : public hf::Board {

    virtual bool getQuaternion(float quat[4]) override
    {
    }

    virtual bool getGyrometer(float gyroRates[3]) override
    {
    }

    virtual void writeMotor(uint8_t index, float value) override
    {
    }

    virtual float getTime(void) override
    {
    }

    virtual uint8_t	serialAvailableBytes(void) override
    {
    }
    
    virtual uint8_t	serialReadByte(void) override
    {
    }

    virtual void serialWriteByte(uint8_t c) override
    {
    }
};


SimFlightController::SimFlightController(void)
{
}

void SimFlightController::initReceiver(uint8_t  axismap[5], uint8_t buttonmap[3], bool reversedVerticals, bool springyThrottle, bool useButtonForAux)
{
    receiver = new hf::SimReceiver(axismap, buttonmap, reversedVerticals, springyThrottle, useButtonForAux);
}

SimFlightController::~SimFlightController(void)
{
}

void SimFlightController::update(void)
{
}

SimFlightController * SimFlightController::createSimFlightController(void)
{
    return new SimFlightController();
}
