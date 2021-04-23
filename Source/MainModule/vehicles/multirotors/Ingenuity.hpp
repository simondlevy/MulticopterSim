/* Helper class for pawns using NASA Ingenuity frame 
 *
 * Copyright (C) 2021 Simon D. Levy 
 *
 * MIT License 
 */ 

#pragma once 


#include "../Multirotor.hpp"

#include "../../dynamics/QuadXAP.hpp" // XXX use quadrotor dynamics for now

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FBodyStatics, "Ingenuity/Body.Body", BodyStatics)
DECLARE_STATIC_MESH(FRotor_BottomStatics, "Ingenuity/Rotor_Bottom.Rotor_Bottom", Rotor_BottomStatics)
DECLARE_STATIC_MESH(FRotor_TopStatics, "Ingenuity/Rotor_Top.Rotor_Top", Rotor_TopStatics)
DECLARE_STATIC_MESH(FLegBottomStatics, "Ingenuity/Leg_Bottom.Leg_Bottom", LegBottomStatics)
DECLARE_STATIC_MESH(FMastStatics, "Ingenuity/Mast.Mast", MastStatics)
DECLARE_STATIC_MESH(FLeg_BracketStatics, "Ingenuity/Leg_Bracket.Leg_Bracket", Leg_BracketStatics)

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

        void addLeg(uint8_t index, int8_t dx, int8_t dy)
        {
            static constexpr float pos = .09;
            
            vehicle.addComponent(
                    Leg_BracketStatics.mesh.Get(), 
                    makeName("Bracket", index, "Mesh"), 
                    dx * pos,
                    dy * pos,
                    .110, 
                    (index-1)*90 + 45);

            vehicle.addComponent(
                    LegBottomStatics.mesh.Get(), 
                    makeName("LegBottom", index, "Mesh"), 
                    dx * pos,
                    dy * pos,
                    .000,
                    (index-1)*90 + 45);
        }

    public:

        void build(APawn * pawn)
        {
            // Build the frame
            //vehicle.buildFull(pawn, BodyStatics.mesh.Get()); // Restore for cameras, audio
            vehicle.build(pawn, BodyStatics.mesh.Get());

            // Add rotors
            addRotor(Rotor_TopStatics.mesh.Get(), .170);
            addRotor(Rotor_BottomStatics.mesh.Get(), .130);
            
            // Add mast, legs, etc.
            vehicle.addMesh(MastStatics.mesh.Get(), "Mast", FVector(0, 0, .135), FRotator(0, 0, 0));
            addLeg(1, +1, -1); 
            //addLeg(2, +1, +1); 
            //addLeg(3, -1, +1); 
            //addLeg(4, -1, -1); 


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
