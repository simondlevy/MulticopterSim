/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "../Vehicle.hpp"
#include "../FlightManager.hpp"

#include "../dynamics/fixedpitch/QuadXBF.hpp"

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "TinyWhoop.generated.h"

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

UCLASS(Config=Game)
class ATinyWhoop : public APawn {

    private:

        GENERATED_BODY()

        Dynamics::vehicle_params_t vparams = {

            // Estimated
            2.E-06, // d drag coefficient [T=d*w^2]

            // https://www.dji.com/phantom-4/info
            1.380,  // m mass [kg]

            // Estimated
            2,      // Ix [kg*m^2] 
            2,      // Iy [kg*m^2] 
            3,      // Iz [kg*m^2] 
            38E-04, // Jr prop inertial [kg*m^2] 
            15000,  // maxrpm
        };

        FixedPitchDynamics::fixed_pitch_params_t fparams = {
            5.E-06, // b thrust coefficient [F=b*w^2]
            0.350   // l arm length [m]
        };

        Camera camera;

        QuadXBFDynamics dynamics = QuadXBFDynamics(vparams, fparams);

        Vehicle vehicle = Vehicle(&dynamics);

        void addMotor(UStaticMesh * motorMesh, uint8_t id);

        void addRotor(float x, float y);

     protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        ATinyWhoop();


}; // ATinyWhoop
