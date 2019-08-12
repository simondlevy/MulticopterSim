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
DECLARE_STATIC_MESH(FProp1Statics, "Phantom/Prop.Prop", Prop1Statics)
DECLARE_STATIC_MESH(FProp2Statics, "Phantom/Prop.Prop", Prop2Statics)
DECLARE_STATIC_MESH(FProp3Statics, "Phantom/Prop.Prop", Prop3Statics)
DECLARE_STATIC_MESH(FProp4Statics, "Phantom/Prop.Prop", Prop4Statics)

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

        void addProp(uint8_t index, int8_t x, int8_t y, UStaticMesh * mesh)
        {
            float d = 0.12;

            vehicle.addProp(index-1, x*d, y*d, +.15, mesh);
        }

    public:

        void build(APawn * pawn)
        {
            vehicle.buildWithAudio(pawn, FrameStatics.mesh.Get());

            // Add propellers
            addProp(1, +1, +1, Prop1Statics.mesh.Get());
            addProp(2, -1, -1, Prop2Statics.mesh.Get());
            addProp(3, +1, -1, Prop3Statics.mesh.Get());
            addProp(4, -1, +1, Prop4Statics.mesh.Get());

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

        void Tick(void)
        {
            vehicle.Tick();
        }

        void addCamera(Camera * camera)
        {
            vehicle.addCamera(camera);
        }

}; // class Phantom 
