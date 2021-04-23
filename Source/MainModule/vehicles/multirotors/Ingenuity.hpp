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
DECLARE_STATIC_MESH(FRotorBottomStatics, "Ingenuity/Rotor_Bottom.Rotor_Bottom", RotorBottomStatics)
DECLARE_STATIC_MESH(FRotorTopStatics, "Ingenuity/Rotor_Top.Rotor_Top", RotorTopStatics)
DECLARE_STATIC_MESH(FLegBottomStatics, "Ingenuity/Leg_Bottom.Leg_Bottom", LegBottomStatics)
DECLARE_STATIC_MESH(FLegBracketStatics, "Ingenuity/Leg_Bracket.Leg_Bracket", LegBracketStatics)
DECLARE_STATIC_MESH(FLegTopStatics, "Ingenuity/Leg_Top.Leg_Top", LegTopStatics)
DECLARE_STATIC_MESH(FMastStatics, "Ingenuity/Mast.Mast", MastStatics)

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

        void addLegComponent(
                UStaticMesh * mesh, 
                const char * name, 
                float xypos, 
                float zpos, 
                float angle,
                uint8_t index, 
                int8_t dx, 
                int8_t dy)
        {
            vehicle.addComponent(mesh, makeName(name, index, "Mesh"), dx * xypos, dy * xypos, zpos, (index-1)*90 + angle);
        }


        void addLeg(uint8_t index, int8_t dx, int8_t dy)
        {
            addLegComponent(LegBracketStatics.mesh.Get(), "LegBracket", .090, +.110, +45, index, dx, dy);
            addLegComponent(LegTopStatics.mesh.Get(),     "LegTop",     .120, +.098, -45, index, dx, dy);
            // addLegComponent(LegBottomStatics.mesh.Get(),  "LegBottom",  .240, -.110, +270, index, dx, dy);
        }

    public:

        void build(APawn * pawn)
        {
            // Build the frame
            //vehicle.buildFull(pawn, BodyStatics.mesh.Get()); // Restore for cameras, audio
            vehicle.build(pawn, BodyStatics.mesh.Get());

            // Add rotors
            addRotor(RotorTopStatics.mesh.Get(), .170);
            addRotor(RotorBottomStatics.mesh.Get(), .130);
            
            // Add mast, legs, etc.
            vehicle.addMesh(MastStatics.mesh.Get(), "Mast", FVector(0, 0, .135), FRotator(0, 0, 0));
            addLeg(1, +1, -1); 
            addLeg(2, +1, +1); 
            addLeg(3, -1, +1); 
            addLeg(4, -1, -1); 

            vehicle.addComponent(
                    LegBottomStatics.mesh.Get(),
                    makeName("LegBottom", 1, "Mesh"),
                    +.216,  // x
                    -.225,  // y
                    -.112,  // z
                    -90);

            vehicle.addComponent(
                    LegBottomStatics.mesh.Get(),
                    makeName("LegBottom", 2, "Mesh"),
                    +.225,  // x
                    +.216,  // y
                    -.112,  // z
                    0);

            vehicle.addComponent(
                    LegBottomStatics.mesh.Get(),
                    makeName("LegBottom", 3, "Mesh"),
                    -.216,  // x
                    +.225,  // y
                    -.112,  // z
                    +90);

            vehicle.addComponent(
                    LegBottomStatics.mesh.Get(),
                    makeName("LegBottom", 4, "Mesh"),
                    -.225,  // x
                    -.216,  // y
                    -.112,  // z
                    +180);

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
