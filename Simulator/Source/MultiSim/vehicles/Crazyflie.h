/*
* Class declaration for Crazyflie pawn class using UDP sockets
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "../Vehicle.hpp"

#include "../dynamics/fixedpitch/QuadXBF.hpp"

#include "Crazyflie.generated.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FPcbStatics, "Crazyflie/pcb.pcb", PcbStatics)

DECLARE_STATIC_MESH(FPropCCWStatics, "Crazyflie/propeller1.propeller1", 
        Propeller1Statics)

DECLARE_STATIC_MESH(FMotorMount1Statics,
        "Crazyflie/motor_mount1.motor_mount1", MotorMount1Statics)

DECLARE_STATIC_MESH(FMotor1Statics,
        "Crazyflie/motor1.motor1", Motor1Statics)

UCLASS(Config=Game)
class ACrazyflie : public APawn {

    private:

        GENERATED_BODY()

        // XXX for DJI Phantom
        Dynamics::vehicle_params_t vparams = {

                // Estimated
                2.E-06, // d drag cofficient [T=d*w^2]

                // https://www.dji.com/phantom-4/info
                1.380,  // m mass [kg]

                // Estimated
                2,      // Ix [kg*m^2] 
                2,      // Iy [kg*m^2] 
                3,      // Iz [kg*m^2] 
                38E-04, // Jr prop inertial [kg*m^2] 
                15000,  // maxrpm

                20      // maxspeed [m/s]
            };

        // XXX for DJI Phantom
        FixedPitchDynamics::fixed_pitch_params_t fparams = {
            5.E-06, // b thrust coefficient [F=b*w^2]
            0.350   // l arm length [m]
        };


        Camera camera;

        QuadXBFDynamics dynamics = QuadXBFDynamics(vparams, fparams);

        Vehicle vehicle = Vehicle(&dynamics);

        void addArm(
                uint8_t index,
                UStaticMesh * motorMountMesh,
                UStaticMesh * motorMesh,
                UStaticMesh * propellerMesh);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        ACrazyflie();

}; // ACrazyflie
