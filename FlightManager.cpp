/*
   MulticopterSim FlightManager class implementation using a stub

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MulticopterSim/FlightManager.hpp"

#include <hackflight.hpp>
#include "SimReceiver.hpp"

#include "SimBoard.hpp"
#include "SimSensors.hpp"

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/althold.hpp>
#include <pidcontrollers/flowhold.hpp>

// Mixer
#include <mixers/quadxap.hpp>

class FNullFlightManager : public FFlightManager {

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
        hf::MixerQuadXAP _mixer;

        // "Sensors" (get values from dynamics)
        SimSensors * _sensors = NULL;

        // Gimbal axes
        float _gimbalRoll = 0;
        float _gimbalPitch = 0;
        float _gimbalFOV = 0;

    public:

        // Constructor
        FNullFlightManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation) 
            : FFlightManager(dynamics, initialLocation, initialRotation) 
        {
        }

        virtual ~FNullFlightManager(void)
        {
        }

        virtual void update(const double time, const MultirotorDynamics::state_t & state, double * motorvals) override
        {
            motorvals[0] = 0.1;
        }

}; // NullFlightManager


// Factory method for FlightManager class
SIMPLUGIN_API FFlightManager * createFlightManager(MultirotorDynamics * dynamics, FVector initialLocation, FRotator initialRotation)
{
    return new FNullFlightManager(dynamics, initialLocation, initialRotation);
}
