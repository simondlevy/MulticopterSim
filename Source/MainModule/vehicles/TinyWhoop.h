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
DECLARE_STATIC_MESH(FMotor1Statics, "TinyWhoop/Motor1.Motor1", Motor1Statics)
DECLARE_STATIC_MESH(FMotor2Statics, "TinyWhoop/Motor2.Motor2", Motor2Statics)
DECLARE_STATIC_MESH(FMotor3Statics, "TinyWhoop/Motor3.Motor3", Motor3Statics)
DECLARE_STATIC_MESH(FMotor4Statics, "TinyWhoop/Motor4.Motor4", Motor4Statics)
DECLARE_STATIC_MESH(FBatteryStatics,"TinyWhoop/Battery.Battery", BatteryStatics)
DECLARE_STATIC_MESH(FCameraMountStatics,  "TinyWhoop/CameraMount.CameraMount", CameraMountStatics)
DECLARE_STATIC_MESH(FCameraStatics,  "TinyWhoop/Camera.Camera", CameraStatics)

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

        // Adds simulated motor barrel to frame
        void addMotor(UStaticMesh * motorMesh, uint8_t id)
        {
            char meshName[10];
            SPRINTF(meshName, "Motor%d", id);
            vehicle.addMesh(motorMesh, meshName);
        }

    public:

        void build(APawn * pawn)
        {
            // Build the frame
            vehicle.buildWithAudio(pawn, FrameStatics.mesh.Get());

            // Add propellers
            vehicle.addProp(Prop1Statics.mesh.Get());
            vehicle.addProp(Prop2Statics.mesh.Get());
            vehicle.addProp(Prop3Statics.mesh.Get());
            vehicle.addProp(Prop4Statics.mesh.Get());

            // Add motor barrels
            addMotor(Motor1Statics.mesh.Get(), 1);
            addMotor(Motor2Statics.mesh.Get(), 2);
            addMotor(Motor3Statics.mesh.Get(), 3);
            addMotor(Motor4Statics.mesh.Get(), 4);

            // Add battery and camera
            vehicle.addMesh(BatteryStatics.mesh.Get(), "BatteryMesh");
            vehicle.addMesh(CameraMountStatics.mesh.Get(), "CameraMountMesh");
            vehicle.addMesh(CameraStatics.mesh.Get(), "CameraMesh");

            // Flight manager will be set in BeginPlay()
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


}; // class TinyWhoop 
