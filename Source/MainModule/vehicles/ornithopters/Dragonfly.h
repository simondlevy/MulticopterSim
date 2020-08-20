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
#include "../MainModule/dynamics/DragonflyDynamics.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FBodyStatics, "Dragonfly/Body.Body", BodyStatics)
DECLARE_STATIC_MESH(FWingStatics, "Dragonfly/Wing.Wing", WingStatics)
DECLARE_STATIC_MESH(FHingeStatics, "Dragonfly/Hinge.Hinge", HingeStatics)
DECLARE_STATIC_MESH(FEyeLeftStatics, "Dragonfly/EyeLeft.EyeLeft", EyeLeftStatics)
DECLARE_STATIC_MESH(FEyeRightStatics, "Dragonfly/EyeRight.EyeRight", EyeRightStatics)

class Dragonfly {

    private:

        Dynamics::Parameters params = Dynamics::Parameters(
                // Estimated
                5.E-06, // b force constatnt [F=b*w^2]
                2.E-06, // d torque constant [T=d*w^2]

                // https://www.dji.com/phantom-4/info
                1.380,  //  m mass [kg]
                0.350,  //  l arm length [m]

                // Estimated
                2,      // Ix [kg*m^2] 
                2,      // Iy [kg*m^2] 
                3,      // Iz [kg*m^2] 
                38E-04, // Jr prop inertial [kg*m^2] 

                15000); // maxrpm

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

        void addWing(float hingeX, float hingeY, float startAngle, float relativeAngle)
        {
            ornithopter.addWing(WingStatics.mesh.Get(), HingeStatics.mesh.Get(), hingeX, hingeY, +0.3, startAngle, relativeAngle);
        }

    public:

        DragonflyDynamics dynamics = DragonflyDynamics(&params);

        Ornithopter ornithopter = Ornithopter(&dynamics);

        void build(APawn * pawn)
        {
            ornithopter.buildFull(pawn, BodyStatics.mesh.Get(), 1.5, 0.5);

            addWing(+0.20, +0.05, -20,  -20);
            addWing(+0.15, -0.05, +160, -20);
            addWing(+0.25, -0.05, -160, +20);
            addWing(+0.10, +0.05, +20,  +20);

            ornithopter.addMesh(EyeLeftStatics.mesh.Get(),  "EyeLeft");
            ornithopter.addMesh(EyeRightStatics.mesh.Get(), "EyeRight");

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
