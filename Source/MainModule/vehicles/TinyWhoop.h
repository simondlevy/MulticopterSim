/* Helper class for pawns using DJI TinyWhoop frame 
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
DECLARE_STATIC_MESH(FPropCWStatics, "TinyWhoop/PropCW.PropCW", PropCWStatics)
DECLARE_STATIC_MESH(FPropCCWStatics, "TinyWhoop/PropCCW.PropCCW", PropCCWStatics)
DECLARE_STATIC_MESH(FMotor1Statics, "TinyWhoop/Motor1.Motor1", Motor1Statics)
DECLARE_STATIC_MESH(FMotor2Statics, "TinyWhoop/Motor2.Motor2", Motor2Statics)
DECLARE_STATIC_MESH(FMotor3Statics, "TinyWhoop/Motor3.Motor3", Motor3Statics)
DECLARE_STATIC_MESH(FMotor4Statics, "TinyWhoop/Motor4.Motor4", Motor4Statics)
DECLARE_STATIC_MESH(FBatteryStatics,"TinyWhoop/Battery.Battery", BatteryStatics)
DECLARE_STATIC_MESH(FCameraMountStatics,  "TinyWhoop/CameraMount.CameraMount", CameraMountStatics)
DECLARE_STATIC_MESH(FCameraStatics,  "TinyWhoop/Camera.Camera", CameraStatics)
DECLARE_STATIC_MESH(FWhoopFCStatics, "TinyWhoop/WhoopFC.WhoopFC", WhoopFCStatics)
DECLARE_STATIC_MESH(FScrew1Statics,  "TinyWhoop/Screw1.Screw1", Screw1Statics)
DECLARE_STATIC_MESH(FScrew2Statics,  "TinyWhoop/Screw2.Screw2", Screw2Statics)
DECLARE_STATIC_MESH(FScrew3Statics,  "TinyWhoop/Screw3.Screw3", Screw3Statics)
DECLARE_STATIC_MESH(FScrew4Statics,  "TinyWhoop/Screw4.Screw4", Screw4Statics)

class TinyWhoop {

    private:

        MultirotorDynamics::Parameters params = MultirotorDynamics::Parameters(

                // Estimated
                5.E-08, // b
                2.E-08, // d

                0.028,  // m (kg)
                0.0275, // l (meters)

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
            vehicle.addProp(PropCCWStatics.mesh.Get(), +.095, +.0725);
            vehicle.addProp(PropCCWStatics.mesh.Get(), +.005, -.02);
            vehicle.addProp(PropCWStatics.mesh.Get(),  +.015, -.02);
            vehicle.addProp(PropCWStatics.mesh.Get(),  -.075, +.0725);

            // Add motor barrels
            addMotor(Motor1Statics.mesh.Get(), 1);
            addMotor(Motor2Statics.mesh.Get(), 2);
            addMotor(Motor3Statics.mesh.Get(), 3);
            addMotor(Motor4Statics.mesh.Get(), 4);

            // Add battery, camera, etc.
            vehicle.addMesh(BatteryStatics.mesh.Get(), "BatteryMesh");
            vehicle.addMesh(CameraMountStatics.mesh.Get(), "CameraMountMesh");
            vehicle.addMesh(CameraStatics.mesh.Get(), "CameraMesh");
            vehicle.addMesh(WhoopFCStatics.mesh.Get(), "WhoopFCMesh");
            vehicle.addMesh(Screw1Statics.mesh.Get(), "Screw1Mesh");
            vehicle.addMesh(Screw2Statics.mesh.Get(), "Screw2Mesh");
            vehicle.addMesh(Screw3Statics.mesh.Get(), "Screw3Mesh");
            vehicle.addMesh(Screw4Statics.mesh.Get(), "Screw4Mesh");

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
