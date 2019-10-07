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

        void addWing(UStaticMesh * wingMesh, UStaticMesh * hingeMesh, float x, float y, float angle)
        {
            UStaticMeshComponent* hingeMeshComponent =
                _pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Hinge", _propCount, "Mesh"));
            hingeMeshComponent->SetStaticMesh(hingeMesh);
            hingeMeshComponent->SetupAttachment(_frameMeshComponent, USpringArmComponent::SocketName);
            hingeMeshComponent->AddRelativeLocation(FVector(x, y, 0) * 100); // m => cm
            _propellerMeshComponents[_propCount] = hingeMeshComponent;
            setPropRotation(_propCount, angle);

            UStaticMeshComponent* wingMeshComponent =
                _pawn->CreateDefaultSubobject<UStaticMeshComponent>(makeName("Wing", _propCount, "Mesh"));
            wingMeshComponent->SetStaticMesh(wingMesh);
            wingMeshComponent->SetupAttachment(hingeMeshComponent, USpringArmComponent::SocketName);
            wingMeshComponent->AddRelativeLocation(FVector(0, +0.2, 0) * 100); // m => cm
  
            _propCount++;
        }

        virtual void setPropRotation(uint8_t index, float angle) override
        {
            // XXX Remove guard once all wings are working
            if (_propellerMeshComponents[index]) {
                _propellerMeshComponents[index]->SetRelativeRotation(FRotator(0, 0, 45*sin(angle/100)));
            }
        }


};  // class Ornithopter
