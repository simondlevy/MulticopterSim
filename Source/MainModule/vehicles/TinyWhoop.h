/*
 Helper class for pawns using DJI TinyWhoop frame
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
DECLARE_STATIC_MESH(FFrameStatics, "TinyWhoop/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FProp1Statics, "TinyWhoop/Prop1.Prop1", Prop1Statics)
DECLARE_STATIC_MESH(FProp2Statics, "TinyWhoop/Prop2.Prop2", Prop2Statics)
DECLARE_STATIC_MESH(FProp3Statics, "TinyWhoop/Prop3.Prop3", Prop3Statics)
DECLARE_STATIC_MESH(FProp4Statics, "TinyWhoop/Prop4.Prop4", Prop4Statics)

class TinyWhoop {

    private:

        MultirotorDynamics::Parameters params = MultirotorDynamics::Parameters(

                // Estimated
                5.E-06, // b
                2.E-06, // d

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

        void addProp(uint8_t index, UStaticMesh * mesh)
        {
            vehicle.addProp(index-1, 0, 0, 0, mesh);
        }


    public:

        void build(APawn * pawn)
        {
            //vehicle.buildWithAudio(pawn, FrameStatics.mesh.Get());
            vehicle.buildWithAudio(pawn, FrameStatics.mesh.Get());

            float y14 = +.0725;
            float y23 = -.0200;

            // Add propellers
            addProp(1, Prop1Statics.mesh.Get());
            addProp(2, Prop2Statics.mesh.Get());
            addProp(3, Prop3Statics.mesh.Get());
            addProp(4, Prop4Statics.mesh.Get());

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


}; // class TinyWhoop 
