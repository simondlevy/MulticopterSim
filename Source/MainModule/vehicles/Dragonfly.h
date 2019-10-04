/*
 Helper class for pawns using DJI Dragonfly frame
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../MainModule/Vehicle.hpp"
#include "../MainModule/Camera.hpp"
#include "../MainModule/dynamics/QuadXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FBodyStatics, "Dragonfly/Body.Body", BodyStatics)
DECLARE_STATIC_MESH(FWingStatics, "Dragonfly/Wing.Wing", WingStatics)

class Dragonfly {

    private:

        MultirotorDynamics::Parameters params = MultirotorDynamics::Parameters(

                // Estimated
                5.E-06, // b
                2.E-06, // d

                // https://www.dji.com/phantom-4/info
                1.380,  // m (kg)
                0.350,  // l (meters)

                // Estimated
                2,      // Ix
                2,      // Iy
                3,      // Iz
                38E-04, // Jr
                15000); // maxrpm

    public:

        QuadXAPDynamics dynamics = QuadXAPDynamics(&params);

        Vehicle vehicle = Vehicle(&dynamics);

    private:

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

    public:

        void build(APawn * pawn)
        {
            vehicle.buildFull(pawn, BodyStatics.mesh.Get(), 1.5, 0.5);

            vehicle.addProp(WingStatics.mesh.Get(), +.5, +.06, 115);

            // Add wings
            //addWing(+1, +1, 90);
            //addWing(-1, -1, 45);
            //addWing(+1, -1, 45);
            //addWing(-1, +1, 45);

            _flightManager = NULL;
        }

        void PostInitializeComponents()
        {
            vehicle.PostInitializeComponents();
        }

        void BeginPlay(FFlightManager * flightManager)
        {
            _flightManager = flightManager;

            vehicle.BeginPlay(flightManager);
        }

        void EndPlay(void)
        {
            FThreadedManager::stopThread((FThreadedManager **)&_flightManager);
        }

        void Tick(float DeltaSeconds)
        {
            vehicle.Tick(DeltaSeconds);
        }

        void addCamera(Camera * camera)
        {
            vehicle.addCamera(camera);
        }

        void addWing(int8_t dx, int8_t dy, float angle)
        {
            vehicle.addProp(WingStatics.mesh.Get(), dx*0.12, dy*0.12, angle);
        }

}; // class Dragonfly 
