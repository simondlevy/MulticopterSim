/*
   MulticopterSim FlightManager class using Hackflight

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"
#include "../MainModule/Transforms.hpp"

#include <hackflight.hpp>

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/rate.hpp>
#include <pidcontrollers/althold.hpp>

#include "SimReceiver.hpp"
#include "SimBoard.hpp"
#include "SimMotor.hpp"

class FHackflightFlightManager : public FFlightManager {

    private:

        // PID tuning

		// Rate
		hf::RatePid ratePid = hf::RatePid(
			.01,	// Kp_roll_pitch
			.01,	// Ki_roll_pitch
			.01,	// Kd_roll_pitch
			.025,	// Kp_yaw 
			.01); 	// Ki_yaw

        // Level
        hf::LevelPid levelPid = hf::LevelPid(0.8);

        // Alt-hold
        hf::AltitudeHoldPid althold = hf::AltitudeHoldPid(
                10.00f, // altHoldPosP
                1.00f,  // altHoldVelP
                0.01f,  // altHoldVelI
                0.10f); // altHoldVelD

        // Flight-controller board
        SimBoard _board;

        // "Receiver" (joystick/gamepad)
        SimReceiver _receiver;

        // Helps us access individual motors
        SimMotor* _motors = NULL;
        uint8_t _nmotors = 0;

        // Main firmware
        hf::Hackflight * _hackflight = NULL;

    public:

        // Constructor
        FHackflightFlightManager(hf::Mixer * mixer, SimMotor * motors, int nmotors, Dynamics * dynamics, bool pidsEnabled=true) 
            : FFlightManager(dynamics, nmotors) 
        {
            _motors = motors;
            _nmotors = nmotors;

            //hf::Hackflight _hackflight(&_board, &_receiver, _mixer);

            // Start Hackflight firmware, indicating already armed
            //_hackflight.begin(true);

            // Add simulated sensor suite
            //_sensors = new SimSensors(_dynamics);
            //_hackflight.addSensor(_sensors);

            if (pidsEnabled) {

                // Add altitude-hold PID controller in switch position 1 or greater
                //_hackflight.addPidController(&althold, 1);

                // Add rate and level PID controllers for all aux switch positions
                //_hackflight.addPidController(&levelPid);
                //_hackflight.addPidController(&ratePid);
            }
        }

        virtual ~FHackflightFlightManager(void)
        {
        }

        virtual void getMotors(const double time, double * motorvals) override
        {
            uint16_t joystickError = _receiver.update();

            double angularVel[3] = {
                _dynamics->x(Dynamics::STATE_PHI_DOT),
                _dynamics->x(Dynamics::STATE_THETA_DOT),
                _dynamics->x(Dynamics::STATE_PSI_DOT) 
            };

            double eulerAngles[3] = {
                _dynamics->x(Dynamics::STATE_PHI),
                _dynamics->x(Dynamics::STATE_THETA),
                _dynamics->x(Dynamics::STATE_PSI) 
            };

            double quaternion[4] = {};
            Transforms::eulerToQuaternion(eulerAngles, quaternion);

            switch (joystickError) {

                case 0:

                    //_hackflight.update();

                    _board.set(time);

                    // _imu.set(quaternion, angularVel);

                    // XXX Get motor values
                    //for (uint8_t i=0; i < _nmotors; ++i) {
                    //    motorvals[i] = _motors->getValue(i);
                    //}

                    break;

                case 1:
                    debug("*** NO JOYSTICK DETECTED ***");
                    break;

                default:
                    debug("*** JOYSTICK 0x%04X NOT RECOGNIZED ***", joystickError);
                    break;

            }
        }

}; // HackflightFlightManager
