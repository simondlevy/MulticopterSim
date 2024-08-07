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

// Should use Crazyflie dynamics
#include "../dynamics/fixedpitch/QuadXBF.hpp"

#include "Crazyflie.generated.h"

// Structures to hold static mesh initializations

DECLARE_STATIC_MESH(FPcbStatics, "Crazyflie/pcb.pcb", PcbStatics)

DECLARE_STATIC_MESH(FBatteryStatics, "Crazyflie/battery.battery", 
        BatteryStatics)

DECLARE_STATIC_MESH(FBatteryHolderStatics,
        "Crazyflie/battery_holder.battery_holder", BatteryHolderStatics)

DECLARE_STATIC_MESH(FPinHeadersRightStatics,
        "Crazyflie/pin_headers_right.pin_headers_right", PinHeadersRightStatics)

DECLARE_STATIC_MESH(FPinHeadersLeftStatics,
        "Crazyflie/pin_headers_left.pin_headers_left", PinHeadersLeftStatics)

DECLARE_STATIC_MESH(FMotorMount1Statics,
        "Crazyflie/motor_mount1.motor_mount1", MotorMount1Statics)

DECLARE_STATIC_MESH(FMotor1Statics,
        "Crazyflie/motor1.motor1", Motor1Statics)

DECLARE_STATIC_MESH(FMotorMount2Statics,
        "Crazyflie/motor_mount2.motor_mount2", MotorMount2Statics)

DECLARE_STATIC_MESH(FMotor2Statics,
        "Crazyflie/motor2.motor2", Motor2Statics)

DECLARE_STATIC_MESH(FMotorMount3Statics,
        "Crazyflie/motor_mount3.motor_mount3", MotorMount3Statics)

DECLARE_STATIC_MESH(FMotor3Statics,
        "Crazyflie/motor3.motor3", Motor3Statics)

DECLARE_STATIC_MESH(FMotorMount4Statics,
        "Crazyflie/motor_mount4.motor_mount4", MotorMount4Statics)

DECLARE_STATIC_MESH(FMotor4Statics,
        "Crazyflie/motor4.motor4", Motor4Statics)

DECLARE_STATIC_MESH(FPropCWStatics,
        "Crazyflie/propeller_cw.propeller_cw", PropCWStatics)

DECLARE_STATIC_MESH(FPropCCWStatics,
        "Crazyflie/propeller_ccw.propeller_ccw", PropCCWStatics)

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
                UStaticMesh * propellerMesh,
                const float propellerX,
                const float propellerY);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        ACrazyflie();

}; // ACrazyflie
