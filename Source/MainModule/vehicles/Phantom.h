/*
 Helper class for pawns using DJI Phantom frame
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
DECLARE_STATIC_MESH(FFrameStatics, "Phantom/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FProp1Statics, "Phantom/Prop1.Prop1", Prop1Statics)
DECLARE_STATIC_MESH(FProp2Statics, "Phantom/Prop2.Prop2", Prop2Statics)
DECLARE_STATIC_MESH(FProp3Statics, "Phantom/Prop3.Prop3", Prop3Statics)
DECLARE_STATIC_MESH(FProp4Statics, "Phantom/Prop4.Prop4", Prop4Statics)

class Phantom {

    private:

        MultirotorDynamics::Parameters params = MultirotorDynamics::Parameters(

                // Estimated
                5.E-06, // b
                2.E-06, // d

                // https://www.dji.com/phantom-4/info
                1.380,  // m
                0.350,  // l

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
            vehicle.buildWithAudio(pawn, FrameStatics.mesh.Get());

            // Add propellers
            vehicle.addProp(Prop1Statics.mesh.Get());
            vehicle.addProp(Prop2Statics.mesh.Get());
            vehicle.addProp(Prop3Statics.mesh.Get());
            vehicle.addProp(Prop4Statics.mesh.Get());

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

}; // class Phantom 
