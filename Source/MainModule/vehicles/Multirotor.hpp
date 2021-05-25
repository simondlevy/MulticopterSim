/*
 * General support for multirotor vehicles in MulticopterSim
 *
 * Copyright (C) 2020 Simon D. Levy, Daniel Katzav
 *
 * MIT License
 */

#pragma once

#include "../Vehicle.hpp"

class MultirotorVehicle : public Vehicle {

    public:

        MultirotorVehicle(Dynamics* dynamics, uint8_t nmotors) 
            : Vehicle(dynamics)
        {
            _nmotors = nmotors;
        }

        UStaticMeshComponent * addComponent(UStaticMesh * mesh, FName name, float x=0, float y=0, float z=0, float yaw_angle=0)
        {
            UStaticMeshComponent* meshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(name);
            meshComponent->SetStaticMesh(mesh);
            meshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName);
            meshComponent->AddRelativeLocation(FVector(x, y, z) * 100); // m => cm
            meshComponent->SetRelativeRotation(FRotator(0, yaw_angle, 0));
            return meshComponent;
        }

        UStaticMeshComponent * addRotor(UStaticMesh* rotorMesh, float x, float y, float z, float angle)
        {
            UStaticMeshComponent * rotorMeshComponent = addComponent(rotorMesh, makeName("Rotor", _rotorCount, "Mesh"), x, y, z, angle);
            _rotorMeshComponents[_rotorCount] = rotorMeshComponent;
            _rotorCount++;
            return rotorMeshComponent;
        }

        void addRotor(UStaticMesh* rotorMesh, float x, float y, float z)
        {
            addRotor(rotorMesh, x, y, z, rotorStartAngle(x,y));
        }

        virtual void setRotorRotation(uint8_t index, float angle)
        {
            _rotorMeshComponents[index]->SetRelativeRotation(FRotator(0, angle, 0));
        }


    protected:

        virtual void animateActuators(void) override
        {
            // Get motor values from dynamics
            _flightManager->getMotorValues(_motorvals);

            // Compute the sum of the motor values
            float motorsum = 0;
            for (uint8_t j = 0; j < _nmotors; ++j) {
                motorsum += _motorvals[j];
            }

            // Rotate rotors. For visual effect, we can ignore actual motor values, and just keep increasing the rotation.
            if (motorsum > 0) {
                rotateRotors(_rotorDirections);
            }

            // Add mean to circular buffer for moving average
            _bufferIndex = _motorBuffer->GetNextIndex(_bufferIndex);
            (*_motorBuffer)[_bufferIndex] = motorsum / _nmotors;

            // Compute the mean motor value over the buffer frames
            float smoothedMotorMean = 0;
            for (uint8_t i = 0; i < _motorBuffer->Capacity(); ++i) {
                smoothedMotorMean += (*_motorBuffer)[i];
            }
            smoothedMotorMean /= _motorBuffer->Capacity();

            // Use the mean motor value to modulate the pitch and voume of the rotor sound
            if (_audioComponent) {
                _audioComponent->SetFloatParameter(FName("pitch"), smoothedMotorMean);
                _audioComponent->SetFloatParameter(FName("volume"), smoothedMotorMean);
            }
        }

    private:

        uint8_t _nmotors = 0;

        float rotorStartAngle(float rotorX, float rotorY)
        {
            FVector vehicleCenter = _pawn->GetActorLocation();
            double theta = -atan2((rotorY - vehicleCenter.Y), (rotorX - vehicleCenter.X));
            return FMath::RadiansToDegrees(M_PI / 2 - theta) + 57.5;
        }

        void rotateRotors(int8_t* rotorDirections)
        {
            static float rotation;
            for (uint8_t i = 0; i < _rotorCount; ++i) {
                setRotorRotation(i, rotation * rotorDirections[i] * 200);
            }
            rotation++;
        }


}; // class MultirotorVehicle
