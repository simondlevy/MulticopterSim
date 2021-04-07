/*
   Event camera simulation thread

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#pragma once

#include "../MainModule/ThreadedManager.hpp"
#include "TargetPawn.h"
#include "dvssim/cpp/Davis346Sim.hpp"
#include "dvssim/cpp//Davis346Sim.hpp"

class FEventCameraManager : public FThreadedManager {

    private:

        APawn * _vehiclePawn = NULL;
        ATargetPawn * _targetPawn = NULL;

        Davis346 * _davis = NULL;

        static const Davis346::Location getLocation(APawn * pawn)
        {
            FVector fv = pawn->GetActorLocation() / 100; // cm => m
            Davis346::Location loc(fv.X, fv.Y, fv.Z);
            return loc;
        }

    public:

        FEventCameraManager(APawn * vehiclePawn, ATargetPawn * targetPawn)
            : FThreadedManager()
        {
            _vehiclePawn = vehiclePawn;
            _targetPawn  = targetPawn;

            FBox targetBox = _targetPawn->getBoundingBox();
            FVector targetSize = targetBox.GetSize() / 100;

            _davis = new Davis346(targetSize.X); // assume a spherical cow
        }

        ~FEventCameraManager()
        {
            delete _davis;
        }

        void performTask(double currentTime)
        {
            _davis->update(getLocation(_vehiclePawn), getLocation(_targetPawn));
        }
}; 
