/*
   Hackflight Receiver subclass for MulticopterSim Allows us to treat an input
   device (joystick, game controller, R/C transmitter) as a "virtual receiver"
   for the firmware.

   Copyright(C) 2019 Simon D.Levy

   MIT License
   */

#pragma once

#include <receiver.hpp>

#include "../MulticopterSim/joystick/Joystick.h"

class SimReceiver : public hf::Receiver {

    friend class FHackflightManager;

    private:

    Joystick * _joystick;

    // Helps mock up periodic availability of new data frame (output data rate; ODR)
    double _deltaT;
    double _previousTime;

    float _gimbalRoll;
    float _gimbalPitch;
    float _gimbalFOV;

    protected:

    uint8_t getAux1State(void) 
    {
        return Receiver::getAux1State();
    }

    uint8_t getAux2State(void)
    {
        // Always armed!
        return 1;
    }


    // Simulate auxiliary switch via pushbuttons
    uint8_t _buttonState;
    const float buttonsToAux[3] = {-.1f, 0.f, .8f};

    public:

    SimReceiver(uint16_t updateFrequency=50)
    {
        _joystick = new Joystick();

        _deltaT = 1./updateFrequency;
        _previousTime = 0;
        _buttonState = 0;
    }

    void begin(void)
    {
    }

    bool gotNewFrame(void)
    {
        // Get a high-fidelity current time value from the OS
        double currentTime = FPlatformTime::Seconds();

        if (currentTime-_previousTime > _deltaT) {
            _previousTime = currentTime;
            return true;
        }

        return false;
    }

    void readRawvals(void)
    {
        // For game controllers, use buttons to fake up values in a three-position aux switch
        if (!_joystick->isRcTransmitter()) {
            rawvals[4] = buttonsToAux[_buttonState];
        }
    }

    Joystick::error_t update(void)
    {
        // Joystick::poll() returns zero (okay) or a postive value (error)
        Joystick::error_t pollResult = _joystick->poll(rawvals, _buttonState);

        // In gimbal mode, grab pan,tilt from cyclic stick, then lock roll and pitch at zero
        if (!pollResult && _joystick->inGimbalMode()) {


            // Get FOV from throttle, gimbal roll and yaw from cyclic stick
            _gimbalFOV   = 90 - rawvals[0] * 45;
            _gimbalRoll  = rawvals[1];
            _gimbalPitch = rawvals[2];


            // Clam throttle, roll, and yaw to neutral values
            rawvals[0] = 0;
            rawvals[1] = 0;
            rawvals[2] = 0;
        }

        return pollResult;
    }

    bool inGimbalMode(void)
    {
        return _joystick->inGimbalMode();
    }

    void getGimbal(float & roll, float & pitch, float & fov)
    {
        roll  = _gimbalRoll;
        pitch = _gimbalPitch;
        fov   = _gimbalFOV;
    }

}; // class SimReceiver
