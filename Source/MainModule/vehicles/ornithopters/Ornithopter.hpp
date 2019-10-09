/*
 * Vehicle subclass for ornithopters
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../MainModule/Vehicle.hpp"

class Ornithopter : public Vehicle {

    public:

        Ornithopter(MultirotorDynamics* dynamics)
            : Vehicle(dynamics)
        {
        }

        virtual ~Ornithopter(void)
        {
        }

        void addWing(UStaticMesh * wingMesh, UStaticMesh * hingeMesh, float hingeX, float hingeY, float wingY, float startAngle, float relativeAngle)
        {
            // Add a new wing structure for animation
            _wings[_propCount].relativeAngle = relativeAngle;
            _wings[_propCount].relativeOffset = (startAngle==relativeAngle) ? 0 : 180;

            // Use a tiny hinge as the "propeller" for this wing
            UStaticMeshComponent* hingeMeshComponent = Vehicle::addProp(hingeMesh, hingeX, hingeY, startAngle);

            // Add the actual wing to the hinge
            UStaticMeshComponent* wingMeshComponent =
                _pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Wing", _propCount, "Mesh"));
            wingMeshComponent->SetStaticMesh(wingMesh);
            wingMeshComponent->SetupAttachment(hingeMeshComponent, USpringArmComponent::SocketName);
            wingMeshComponent->AddRelativeLocation(FVector(+.025, wingY, -.05) * 100); // m => cm
        }

        virtual void setPropRotation(uint8_t index, float angle) override
        {
            wing_t wing = _wings[index];

            _propellerMeshComponents[index]->SetRelativeRotation(FRotator(0, wing.relativeAngle, wing.relativeOffset + 45*sin(angle/10000)));
        }

    private:

        typedef struct {

            float relativeAngle;
            float relativeOffset;

        } wing_t;

        wing_t _wings[FFlightManager::MAX_MOTORS] = {};

};  // class Ornithopter
