/*
 Helper class for pawns using Dragonfly insect frame
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "Ornithopter.hpp"
#include "../MainModule/Camera.hpp"
#include "../MainModule/dynamics/QuadXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FBodyStatics, "Dragonfly/Body.Body", BodyStatics)
DECLARE_STATIC_MESH(FWingStatics, "Dragonfly/Wing.Wing", WingStatics)
DECLARE_STATIC_MESH(FHingeStatics, "Dragonfly/Hinge.Hinge", HingeStatics)

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

        Ornithopter ornithopter = Ornithopter(&dynamics);

    private:

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

    public:

        void build(APawn * pawn)
        {
            ornithopter.buildFull(pawn, BodyStatics.mesh.Get(), 1.5, 0.5);

            ornithopter.addWing(WingStatics.mesh.Get(), HingeStatics.mesh.Get(), +0.125, +0.05, 90);

            _flightManager = NULL;
        }

        void PostInitializeComponents()
        {
            ornithopter.PostInitializeComponents();
        }

        void BeginPlay(FFlightManager * flightManager)
        {
            _flightManager = flightManager;

            ornithopter.BeginPlay(flightManager);
        }

        void EndPlay(void)
        {
            FThreadedManager::stopThread((FThreadedManager **)&_flightManager);
        }

        void Tick(float DeltaSeconds)
        {
            ornithopter.Tick(DeltaSeconds);
        }

        void addCamera(Camera * camera)
        {
            ornithopter.addCamera(camera);
        }

}; // class Dragonfly 
