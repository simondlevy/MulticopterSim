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

        void addWing(UStaticMesh* wingMesh, float x, float y, float angle)
        {
            Vehicle::addProp(wingMesh, x, y, angle);
        }

        virtual void setPropRotation(uint8_t index, float angle) override
        {
            // XXX Remove guard once all wings are working
            if (_propellerMeshComponents[index]) _propellerMeshComponents[index]->SetRelativeRotation(FRotator(0, angle, 0));
        }


};  // class Ornithopter
