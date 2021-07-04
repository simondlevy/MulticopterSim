/*
   MulticopterSim FlightManager class using Hackflight

   Just spins propellers

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../MainModule/FlightManager.hpp"
#include "../MainModule/Dynamics.hpp"

#include <hfpure.hpp>

// Coordinate transform helpers
#include <RFT_filters.hpp>

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/yaw.hpp>
#include <pidcontrollers/rate.hpp>

#include "SimReceiver.hpp"
#include "SimBoard.hpp"
#include "SimSensors.hpp"
#include "SimMotor.hpp"

class FHackflightFlightManager : public FFlightManager {

    private:

        // PID tuning

		// Rate
		hf::RatePid ratePid = hf::RatePid(.01, .01, .01);	

   		// Yaw 
		hf::YawPid yawPid = hf::YawPid(.025, .01);

        // Level
        hf::LevelPid levelPid = hf::LevelPid(1.0);

        // Flight-controller board
        SimBoard _board;

        // "Receiver" (joystick/gamepad)
        SimReceiver * _receiver = NULL;

        // "Sensors"
        SimSensors* _sensors = NULL;

        // "Motors" are passed to mixer so it can modify them
        SimMotor * _motors[100] = {};

        // Main firmware
        hf::HackflightPure * _hackflight = NULL;

    public:

        // Constructor
        FHackflightFlightManager(APawn * pawn, hf::Mixer * mixer, SimMotor ** motors, Dynamics * dynamics) 
            : FFlightManager(dynamics) 
        {
            // Store motors for later
            for (uint8_t k=0; k<_actuatorCount; ++k) {
                _motors[k] = motors[k];
            }

            // Pass PlayerController to receiver constructor in case we have no joystick / game-controller
            _receiver = new SimReceiver(UGameplayStatics::GetPlayerController(pawn->GetWorld(), 0));

            // Create Hackflight object
            _hackflight = new hf::HackflightPure(&_board, _receiver, mixer);

            // Add simulated sensor suite
            _sensors = new SimSensors(_dynamics);
            _hackflight->addSensor(_sensors);

            // Add PID controllers for all aux switch positions
            _hackflight->addClosedLoopController(&levelPid);
            _hackflight->addClosedLoopController(&ratePid);
            _hackflight->addClosedLoopController(&yawPid);

            // Start Hackflight firmware, indicating already armed
            _hackflight->begin(true);
        }

        virtual ~FHackflightFlightManager(void)
        {
            delete _hackflight;
        }

        virtual void getActuators(const double time, double * values) override
        {
            // Zero on success, nonzero otherwise
            uint16_t joystickError = _receiver->update();

            double angularVel[3] = {
                _dynamics->x(Dynamics::STATE_PHI_DOT),
                _dynamics->x(Dynamics::STATE_THETA_DOT),
                _dynamics->x(Dynamics::STATE_PSI_DOT) 
            };

            float eulerAngles[3] = {
                _dynamics->x(Dynamics::STATE_PHI),
                _dynamics->x(Dynamics::STATE_THETA),
                _dynamics->x(Dynamics::STATE_PSI) 
            };

            float quaternion[4] = {};
            rft::Filter::euler2quat(eulerAngles, quaternion);

            _hackflight->update();

            _board.set(time);

            //  Get motor values
            for (uint8_t i=0; i < _actuatorCount; ++i) {
                values[i] = _motors[i]->getValue();
            }
        }

        void tick(void)
        {
            _receiver->tick();
        }

}; // HackflightFlightManager
