/* Helper class for pawns using NASA Ingenuity frame 
 *
 * Copyright (C) 2021 Simon D. Levy 
 *
 * MIT License 
 */ 

#pragma once 

#include "../Multirotor.hpp"
#include "../MainModule/dynamics/QuadXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FBodyStatics, "Ingenuity/Body.Body", BodyStatics)
DECLARE_STATIC_MESH(FRotor_BottomStatics, "Ingenuity/Rotor_Bottom.Rotor_Bottom", Rotor_BottomStatics)
DECLARE_STATIC_MESH(FRotor_TopStatics, "Ingenuity/Rotor_Top.Rotor_Top", Rotor_TopStatics)
DECLARE_STATIC_MESH(FLegStatics, "Ingenuity/Leg.Leg", LegStatics)

class Ingenuity {

    private:

        // Estimated
        static constexpr double b = 5.E-06; // force constatnt [F=b*w^2]
        static constexpr double d = 2.E-06; // torque constant [T=d*w^2]

        // https://www.dji.com/phantom-4/info
        static constexpr double m = 1.380;  // mass [kg]

        // Estimated
        static constexpr double Ix = 2;      // [kg*m^2] 
        static constexpr double Iy = 2;      // [kg*m^2] 
        static constexpr double Iz = 3;      // [kg*m^2] 
        static constexpr double Jr = 38E-04; // prop inertial [kg*m^2] 

        static constexpr double l = 0.350;  // arm length [m]

        static const uint16_t maxrpm = 15000; // maxrpm

    public:

        QuadXAPDynamics dynamics = QuadXAPDynamics(b, d, m, Ix, Iy, Iz, Jr, l, maxrpm);

        MultirotorVehicle vehicle = MultirotorVehicle(&dynamics, 4);

    private:

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

        void addRotor(UStaticMesh* propMesh, float z)
        {
            vehicle.addProp(propMesh, 0, 0, z);
        }

    public:

        void build(APawn * pawn)
        {
            // Build the frame
            //vehicle.buildFull(pawn, BodyStatics.mesh.Get());
            vehicle.build(pawn, BodyStatics.mesh.Get());

            // Add propellers
            addRotor(Rotor_TopStatics.mesh.Get(), .175);
            addRotor(Rotor_BottomStatics.mesh.Get(), .125);

            // Add legs, mast, etc.
            vehicle.addMesh(LegStatics.mesh.Get(), "Leg1", FVector(+.09, +.09, -.06), FRotator(0, 0, 0));
            vehicle.addMesh(LegStatics.mesh.Get(), "Leg2", FVector(-.09, -.09, -.06), FRotator(0, 180, 0));

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


}; // class Ingenuity 
