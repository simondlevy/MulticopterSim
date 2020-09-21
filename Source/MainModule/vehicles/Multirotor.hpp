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

        MultirotorVehicle(Dynamics* dynamics) 
            : Vehicle(dynamics)
        {
        }

        UStaticMeshComponent * addComponent(UStaticMesh * mesh, FName name, float x, float y, float z, float angle)
        {
            UStaticMeshComponent* meshComponent = _pawn->CreateDefaultSubobject<UStaticMeshComponent>(name);
            meshComponent->SetStaticMesh(mesh);
            meshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName);
            meshComponent->AddRelativeLocation(FVector(x, y, z) * 100); // m => cm
            meshComponent->SetRelativeRotation(FRotator(0, angle, 0));
            return meshComponent;
        }

        UStaticMeshComponent * addProp(UStaticMesh* propMesh, float x, float y, float z, float angle)
        {
            UStaticMeshComponent * propMeshComponent = addComponent(propMesh, makeName("Prop", _propCount, "Mesh"), x, y, z, angle);
            _propellerMeshComponents[_propCount] = propMeshComponent;
            _propCount++;
            return propMeshComponent;
        }

        void addProp(UStaticMesh* propMesh, float x, float y, float z)
        {
            addProp(propMesh, x, y, z, propStartAngle(x,y));
        }

        virtual void setPropRotation(uint8_t index, float angle)
        {
            _propellerMeshComponents[index]->SetRelativeRotation(FRotator(0, angle, 0));
        }


    protected:

        virtual void animateActuators(void) override
        {
            // Get motor values from dynamics
            _flightManager->getMotorValues(_motorvals);

            // Compute the sum of the motor values
            float motorsum = 0;
            for (uint8_t j = 0; j < _dynamics->motorCount(); ++j) {
                motorsum += _motorvals[j];
            }

            // Rotate props. For visual effect, we can ignore actual motor values, and just keep increasing the rotation.
            if (motorsum > 0) {
                rotateProps(_motorDirections);
            }

            // Add mean to circular buffer for moving average
            _bufferIndex = _motorBuffer->GetNextIndex(_bufferIndex);
            (*_motorBuffer)[_bufferIndex] = motorsum / _dynamics->motorCount();

            // Compute the mean motor value over the buffer frames
            float smoothedMotorMean = 0;
            for (uint8_t i = 0; i < _motorBuffer->Capacity(); ++i) {
                smoothedMotorMean += (*_motorBuffer)[i];
            }
            smoothedMotorMean /= _motorBuffer->Capacity();

            // Use the mean motor value to modulate the pitch and voume of the propeller sound
            _audioComponent->SetFloatParameter(FName("pitch"), smoothedMotorMean);
            _audioComponent->SetFloatParameter(FName("volume"), smoothedMotorMean);
        }

    private:

        float propStartAngle(float propX, float propY)
        {
            FVector vehicleCenter = _pawn->GetActorLocation();
            double theta = -atan2((propY - vehicleCenter.Y), (propX - vehicleCenter.X));
            return FMath::RadiansToDegrees(3.14159 / 2 - theta) + 57.5;
        }

        void rotateProps(int8_t* motorDirections)
        {
            static float rotation;
            for (uint8_t i = 0; i < _propCount; ++i) {
                setPropRotation(i, rotation * motorDirections[i] * 200);
            }
            rotation++;
        }


}; // class MultirotorVehicle
