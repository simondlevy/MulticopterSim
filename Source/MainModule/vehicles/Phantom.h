/*
 Helper class for pawns using DJI Phantom frame
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../MainModule/Vehicle.hpp"
#include "../MainModule/dynamics/QuadXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Phantom/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FProp1Statics, "Phantom/Prop.Prop", Prop1Statics)
DECLARE_STATIC_MESH(FProp2Statics, "Phantom/Prop.Prop", Prop2Statics)
DECLARE_STATIC_MESH(FProp3Statics, "Phantom/Prop.Prop", Prop3Statics)
DECLARE_STATIC_MESH(FProp4Statics, "Phantom/Prop.Prop", Prop4Statics)

class Phantom {

    private:

        MultirotorDynamics::params_t _params = {

            // XXX These parameters are for a much larger vehicle
            5.30216718361085E-05,   // b
            2.23656692806239E-06,   // d
            16.47,                  // m
            0.6,                    // l
            2,                      // Ix
            2,                      // Iy
            3,                      // Iz
            3.08013E-04,            // Jr

            // maxrpm, estimated
            15000,                  

            0                       // XXX motor acceleration 
        };

        Vehicle _vehicle = Vehicle(&dynamics);

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

        void addProp(uint8_t index, int8_t x, int8_t y, UStaticMesh * mesh)
        {
            float d = 0.12;

            _vehicle.addProp(index-1, x*d, y*d, +.15, mesh);
        }

    public:

        QuadXAPDynamics dynamics = QuadXAPDynamics(_params);

        void build(APawn * pawn)
        {
            _vehicle.build(pawn, FrameStatics.mesh.Get());

            // Add propellers
            addProp(1, +1, +1, Prop1Statics.mesh.Get());
            addProp(2, -1, -1, Prop2Statics.mesh.Get());
            addProp(3, +1, -1, Prop3Statics.mesh.Get());
            addProp(4, -1, +1, Prop4Statics.mesh.Get());

            _flightManager = NULL;
        }

        void PostInitializeComponents()
        {
            _vehicle.PostInitializeComponents();
        }

        void BeginPlay(FFlightManager * flightManager)
        {
            _vehicle.BeginPlay(flightManager);
        }

        void EndPlay(void)
        {
            _flightManager = (FFlightManager *)FThreadedWorker::stopThreadedWorker(_flightManager);
        }

        void Tick(void)
        {
            _vehicle.Tick();
        }

}; // class Phantom 
