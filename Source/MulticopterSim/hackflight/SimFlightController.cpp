/*
 * SimFlightController.cpp: Hackflight flight-control class for MulticopterSim
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#include "SimFlightController.h"

#include "VehiclePawn.h"

#include "Joystick.h"

// Math support
#define _USE_MATH_DEFINES
#include <math.h>

#include <hackflight.hpp>
#include "SimReceiver.h"
//
// MSP comms
#include "msppg/MSPPG.h"

// PID controllers
#include <pidcontrollers/level.hpp>
#include <pidcontrollers/althold.hpp>
#include <pidcontrollers/poshold.hpp>

// Mixer
#include <mixers/quadx.hpp>

// Additional sensors
#include "hackflight/sensors/SimOpticalFlow.h"
#include "hackflight/sensors/SimRangefinder.h"

class HackflightSimFlightController : public SimFlightController, public hf::Board {

    private:

        // PID tuning

        hf::Rate ratePid = hf::Rate(
                0.01,	// Roll/Pitch P
                0.01,	// Roll/Pitch I
                0.01,	// Roll/Pitch D
                0.5,	// Yaw P
                0.0,	// Yaw I
                8.f);	// Demands to rate


        hf::Level level = hf::Level(0.20f);

#ifdef _PYTHON
        PythonLoiter loiter = PythonLoiter(
                0.5f,	// Altitude P
                1.0f,	// Altitude D
                0.2f);	// Cyclic P
#else

        hf::AltitudeHold althold = hf::AltitudeHold(
                1.00f,  // altHoldP
                0.50f,  // altHoldVelP
                0.01f,  // altHoldVelI
                0.10f); // altHoldVelD

        hf::PositionHold poshold = hf::PositionHold(
                0.2,	// posP
                0.2f,	// posrP
                0.0f);	// posrI

#endif

        // Main firmware
        hf::Hackflight hackflight;

        // "Receiver" (joystick/gamepad)
        hf::SimReceiver * receiver;

        // Mixer
        hf::MixerQuadX mixer;

        float _elapsedTime;

        float _quat[4];
        float _gyro[3];

        float _motorvals[3];

		// Support for sensor emulation via first differencing
		FVector _eulerPrev;
		float _varioPrev;
		float _groundAltitude;

		FVector getAccelerometer(float velocityZ, FVector & euler, float deltaSeconds)
		{
			// Use velocity first difference to emulate G force on vehicle in inertial frame
			float vario = velocityZ / 100; // m/s
			float gs = ((vario - _varioPrev) / deltaSeconds + AVehiclePawn::G) / AVehiclePawn::G;
			_varioPrev = vario;

			// Convert inertial frame to body frame
			// See slide 50 from https://slideplayer.com/slide/2813564/
			float phi = euler.X;
			float theta = euler.Y;
			return gs * FVector(-sin(theta), sin(phi)*cos(theta), cos(phi)*cos(theta));
		}

		FQuat getQuaternion(class AVehiclePawn * vehiclePawn)
		{
			// Get current quaternion and convert it to our format (XXX necessary?
			FQuat quat = vehiclePawn->GetActorQuat();
			quat.X = -quat.X;
			quat.Y = -quat.Y;
			return quat;
		}


        // Support for additional sensors
        //SimOpticalFlow _flowSensor = SimOpticalFlow(this);
        //SimRangefinder _rangefinder = SimRangefinder(this);

        // SimFlightController method implementation -----------------------------------

    public:

        HackflightSimFlightController(void)
        {
            // Start the "receiver" (joystick/gamepad)
            receiver = new hf::SimReceiver();

            // Start Hackflight firmware, indicating already armed
            hackflight.init(this, receiver, &mixer, &ratePid, true);

            // Add optical-flow sensor
            //hackflight.addSensor(&_flowSensor);

            // Add rangefinder
            //hackflight.addSensor(&_rangefinder);

            // Add level PID controller for aux switch position 1
            hackflight.addPidController(&level, 1);

            // Add loiter PID controllers for aux switch position 2
            hackflight.addPidController(&althold, 2);
            //hackflight.addPidController(&poshold, 2);
        }

        virtual void start(void) override
        {
			// Initialize simulation variables
			_eulerPrev = FVector(0, 0, 0);
			_varioPrev = 0;
			_elapsedTime = 0;
        }

		virtual TArray<float> update(float deltaSeconds, FVector gyro, AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh) override
		{
			// Update the receiver
			receiver->update();

			// Update the Hackflight firmware
			hackflight.update();

			// Convert quaternion to Euler angles
			FVector euler = FMath::DegreesToRadians(vehiclePawn->GetActorQuat().Euler());

			// Get the simulated IMU readings
			FQuat   quat = getQuaternion(vehiclePawn);

			// Store quaternion and gyro values for Hackflight::Board methods below
			_quat[0] = quat.W;
			_quat[1] = quat.X;
			_quat[2] = quat.Y;
			_quat[3] = quat.Z;
			_gyro[0] = gyro.X;
			_gyro[1] = gyro.Y;
			_gyro[2] = 0; // zero-out gyro Z for now
			
			TArray<float> motorvals = { _motorvals[0], _motorvals[1], _motorvals[2], _motorvals[3] };

			// Use physics model to compute rotation and translation forces on vehicle
			FVector rotationForce = { 0,0,0 };
			FVector translationForce = { 0,0,0 };
			computeForces(deltaSeconds, motorvals, euler, rotationForce, translationForce);

			// Add movement force vector to vehicle 
			vehicleMesh->AddForce(translationForce);

			// Add rotation to vehicle 
			vehiclePawn->AddActorLocalRotation(deltaSeconds * FRotator(rotationForce.Y, rotationForce.Z, rotationForce.X) * (180 / M_PI));
			
			// Output the motor values for audiovisual effect
			return motorvals;
		}

		void computeForces(float deltaSeconds, TArray<float> motorValues, FVector & euler,
			FVector & rotationForce, FVector & translationForce)
		{
			// Convert motor values to rotational forces
			rotationForce.X = motorsToAngularForce(motorValues, 2, 3, 0, 1);
			rotationForce.Y = motorsToAngularForce(motorValues, 1, 3, 0, 2);
			rotationForce.Z = motorsToAngularForce(motorValues, 1, 2, 0, 3);

			// Rotate Euler angles into inertial frame: http://www.chrobotics.com/library/understanding-euler-angles
			float x = sin(euler.X)*sin(euler.Z) + cos(euler.X)*cos(euler.Z)*sin(euler.Y);
			float y = cos(euler.X)*sin(euler.Y)*sin(euler.Z) - cos(euler.Z)*sin(euler.X);
			float z = cos(euler.Y)*cos(euler.X);

			// Use rotated Euler angles to compute translation force
			translationForce = THRUST_FACTOR * sum(motorValues) * FVector(-x, -y, z);
		}

		static constexpr float THRUST_FACTOR = 130.f;

		static float motorsToAngularForce(TArray<float> motorValues, uint8_t a, uint8_t b, uint8_t c, uint8_t d)
		{
			float v = ((motorValues[a] + motorValues[b]) - (motorValues[c] + motorValues[d]));

			return (v < 0 ? -1 : +1) * fabs(v);
		}

		static float sum(TArray<float> x)
		{
			float s = 0.f;

			for (auto it = x.CreateConstIterator(); it; ++it) {
				s += *it;
			}

			return s;
		}

    protected:

        // Hackflight::Board method implementation -------------------------------------

        virtual bool getQuaternion(float quat[4]) override
        {
            memcpy(quat, _quat, 4*sizeof(float));
            return true;
        }

        virtual bool getGyrometer(float gyro[3]) override
        {
            memcpy(gyro, _gyro, 3*sizeof(float));
            return true;
        }

        virtual void writeMotor(uint8_t index, float value) override
        {
            _motorvals[index] = value;
        }

        virtual float getTime(void) override
        {
            // Track elapsed time
            _elapsedTime += .01; // Assume 100Hz clock

            return _elapsedTime;
        }

        virtual uint8_t	serialAvailableBytes(void) override
        {
            return 0; // XXX
        }

        virtual uint8_t	serialReadByte(void) override
        {
            return 0; // XXX
        }

        virtual void serialWriteByte(uint8_t c) override
        { // XXX
        }

}; // HackflightSimFlightController

// Debugging
void hf::Board::outbuf(char * buf)
{
    AVehiclePawn::outbuf(buf);
}

// Factory method
SimFlightController * SimFlightController::createSimFlightController(void)
{
    return new HackflightSimFlightController();
}
