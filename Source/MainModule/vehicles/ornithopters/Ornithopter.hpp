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

    private:

        static constexpr float MAX_FLAP_DEGREES   = 45;
        static constexpr float FLAP_ANGLE_DIVISOR = 100; // bigger = slower

        typedef struct {

            float yawRelative;
            float rollRelative;

        } wing_t;

        wing_t _wings[FFlightManager::MAX_MOTORS] = {};

    public:

        Ornithopter(Dynamics* dynamics)
            : Vehicle(dynamics)
        {
        }

        virtual ~Ornithopter(void)
        {
        }

        void addWing(UStaticMesh * wingMesh, UStaticMesh * hingeMesh, float hingeX, float hingeY, float wingY, float yawStart, float yawRelative)
        {
            // Add a new wing structure for animation
            _wings[_propCount].yawRelative = yawRelative;
            _wings[_propCount].rollRelative = (yawStart==yawRelative) ? 0 : 180;

            // Use a tiny hinge as the "propeller" for this wing
            UStaticMeshComponent* hingeMeshComponent = Vehicle::addProp(hingeMesh, hingeX, hingeY, yawStart);

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

            // Flap wing via sine of angle
            _propellerMeshComponents[index]->SetRelativeRotation(FRotator(0, wing.yawRelative, wing.rollRelative + MAX_FLAP_DEGREES*sin(angle/FLAP_ANGLE_DIVISOR)));
        }

};  // class Ornithopter
