/*
   MulticopterSim FlightManager class implementation using a stub

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"

#include <hackflight.hpp>
#include "SimReceiver.hpp"

#include "SimBoard.hpp"
#include "SimSensors.hpp"

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/althold.hpp>
#include <pidcontrollers/flowhold.hpp>

// Mixer
#include <mixers/octoxap.hpp>

class FHackflightFlightManager : public FFlightManager {

    private:

        // PID tuning

        // Rate
        hf::Rate ratePid = hf::Rate(
                0.01,	// Roll/Pitch P
                0.01,	// Roll/Pitch I
                0.01,	// Roll/Pitch D
                0.025,	// Yaw P
                0.01,	// Yaw I
                8.00);	// Demands to rate

        // Level
        hf::Level level = hf::Level(0.1);

        // Alt-hold
        hf::AltitudeHold althold = hf::AltitudeHold(
                5.00f,  // altHoldP
                1.00f,  // altHoldVelP
                0.01f,  // altHoldVelI
                0.10f); // altHoldVelD

        // Pos-hold (via simulated optical flow)
        hf::FlowHold flowhold = hf::FlowHold(0.01);

        // Main firmware
        hf::Hackflight _hackflight;

        // Flight-controller board
        SimBoard _board;

        // "Receiver" (joystick/gamepad)
        SimReceiver _receiver;

        // Mixer
        hf::MixerOctoXAP _mixer;

        // "Sensors" (get values from dynamics)
        SimSensors * _sensors = NULL;

        // Gimbal axes
        float _gimbalRoll = 0;
        float _gimbalPitch = 0;
        float _gimbalYaw = 0;
        float _gimbalFOV = 0;

    public:

        // Constructor
        FHackflightFlightManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation) 
            : FFlightManager(dynamics, initialLocation, initialRotation) 
        {
            // Start Hackflight firmware, indicating already armed
            _hackflight.init(&_board, &_receiver, &_mixer, &ratePid, true);

            // Add simulated sensor suite
            _sensors = new SimSensors(_dynamics);
            _hackflight.addSensor(_sensors);

            // Add level PID controller for aux switch position 1
            _hackflight.addPidController(&level, 1);

            // Add altitude-hold and position-hold PID controllers in switch position 2
            _hackflight.addPidController(&althold, 2);    
            _hackflight.addPidController(&flowhold, 2);    

            // Start gimbal in center, medium Field-Of-View
            _gimbalRoll = 0;
            _gimbalPitch = 0;
            _gimbalFOV = 90;
        }

        virtual ~FHackflightFlightManager(void)
        {
        }

        virtual void getMotors(const double time, const MultirotorDynamics::state_t & state, double * motorvals) override
        {
            Joystick::error_t joystickError = _receiver.update();

            switch (joystickError) {

                case Joystick::ERROR_MISSING:
                    dbgprintf("*** NO JOYSTICK DETECTED ***");
                    break;

                case Joystick::ERROR_PRODUCT:
                    dbgprintf("*** JOYSTICK NOT RECOGNIZED ***");
                    break;

                default:

                    if (_receiver.inGimbalMode()) {
                        _receiver.getGimbal(_gimbalRoll, _gimbalPitch, _gimbalYaw, _gimbalFOV);
                    }

                    _hackflight.update();

                    // Input deltaT, quat, gyro; output motor values
                    _board.getMotors(time, state.quaternion, state.angularVel, motorvals, 4);
            }
         }

        void getGimbal(float & roll, float &pitch, float & yaw, float & fov)
        {
            roll  = _gimbalRoll;
            pitch = _gimbalPitch;
            yaw   = _gimbalYaw;
            fov   = _gimbalFOV;
        }

}; // HackflightFlightManager

static FHackflightFlightManager * _flightManager;

// Factory method for FlightManager class
FLIGHTMODULE_API FFlightManager * createFlightManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation)
{
    _flightManager = new FHackflightFlightManager(dynamics, initialLocation, initialRotation);
    return _flightManager;
}

void getGimbalFromFlightManager(float & roll, float & pitch, float & yaw, float & fov) 
{
    _flightManager->getGimbal(roll, pitch, yaw, fov);
}
