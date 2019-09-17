/*
   MulticopterSim FlightManager class implementation using a stub

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"

#include <hackflight.hpp>

// PID controllers
#include <pidcontrollers/yaw.hpp>
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/rate.hpp>
#include <pidcontrollers/althold.hpp>
#include <pidcontrollers/flowhold.hpp>

// Mixer
#include <mixers/quadxap.hpp>

#include "SimReceiver.hpp"
#include "SimBoard.hpp"
#include "SimSensors.hpp"

class FHackflightFlightManager : public FFlightManager {

    private:

        // PID tuning

		// Rate
		hf::RatePid ratePid = hf::RatePid(.01, .01, .01);

        // Level
        hf::LevelPid levelPid = hf::LevelPid(0.1);

        // Yaw
        hf::YawPid yawPid = hf::YawPid(.025, .01);

        // Alt-hold
        hf::AltitudeHoldPid althold = hf::AltitudeHoldPid(
                5.00f,  // altHoldP
                1.00f,  // altHoldVelP
                0.01f,  // altHoldVelI
                0.10f); // altHoldVelD

        // Pos-hold (via simulated optical flow)
        hf::FlowHoldPid flowhold = hf::FlowHoldPid(0.01);

        // Main firmware
        hf::Hackflight _hackflight;

        // Flight-controller board
        SimBoard _board;

        // "Receiver" (joystick/gamepad)
        SimReceiver _receiver;

        // Mixer
        hf::MixerQuadXAP _mixer;

        // "Sensors" (get values from dynamics)
        SimSensors * _sensors = NULL;

    public:

        // Constructor
        FHackflightFlightManager(MultirotorDynamics * dynamics) 
            : FFlightManager(dynamics) 
        {
            // Start Hackflight firmware, indicating already armed
            _hackflight.init(&_board, &_receiver, &_mixer, true);

            // Add simulated sensor suite
            _sensors = new SimSensors(_dynamics);
            _hackflight.addSensor(_sensors);

            // Add level and yaw PID controller for all aux-switch psoitions
            _hackflight.addPidController(&levelPid);
            _hackflight.addPidController(&yawPid);

            // Add altitude-hold and position-hold PID controllers in switch position 1
            _hackflight.addPidController(&althold, 1);    
            _hackflight.addPidController(&flowhold, 1);    
        }

        virtual ~FHackflightFlightManager(void)
        {
        }

        virtual void getMotors(const double time, const MultirotorDynamics::state_t & state, double * motorvals) override
        {
            Joystick::error_t joystickError = _receiver.update();

            switch (joystickError) {

                case Joystick::ERROR_MISSING:
                    debug("*** NO JOYSTICK DETECTED ***");
                    break;

                case Joystick::ERROR_PRODUCT:
                    debug("*** JOYSTICK NOT RECOGNIZED ***");
                    break;

                default:

                    _hackflight.update();

                    // Input deltaT, quat, gyro; output motor values
                    _board.getMotors(time, state.quaternion, state.angularVel, motorvals, 4);
            }
         }

}; // HackflightFlightManager
