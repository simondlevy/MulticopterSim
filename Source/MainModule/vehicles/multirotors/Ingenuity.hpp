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
DECLARE_STATIC_MESH(FMastStatics, "Ingenuity/Mast.Mast", MastStatics)
DECLARE_STATIC_MESH(FSolar_PanelStatics, "Ingenuity/Solar_Panel.Solar_Panel", SolarPanelStatics)
DECLARE_STATIC_MESH(FAntennaStatics, "Ingenuity/Antenna.Antenna", AntennaStatics)

DECLARE_STATIC_MESH(FLeg1BottomStatics, "Ingenuity/Leg1_Bottom.Leg1_Bottom", Leg1BottomStatics)
DECLARE_STATIC_MESH(FLeg1BracketStatics, "Ingenuity/Leg1_Bracket.Leg1_Bracket", Leg1BracketStatics)
DECLARE_STATIC_MESH(FLeg1TopStatics, "Ingenuity/Leg1_Top.Leg1_Top", Leg1TopStatics)

DECLARE_STATIC_MESH(FLeg2BottomStatics, "Ingenuity/Leg2_Bottom.Leg2_Bottom", Leg2BottomStatics)
DECLARE_STATIC_MESH(FLeg2BracketStatics, "Ingenuity/Leg2_Bracket.Leg2_Bracket", Leg2BracketStatics)
DECLARE_STATIC_MESH(FLeg2TopStatics, "Ingenuity/Leg2_Top.Leg2_Top", Leg2TopStatics)

DECLARE_STATIC_MESH(FLeg3BottomStatics, "Ingenuity/Leg3_Bottom.Leg3_Bottom", Leg3BottomStatics)
DECLARE_STATIC_MESH(FLeg3BracketStatics, "Ingenuity/Leg3_Bracket.Leg3_Bracket", Leg3BracketStatics)
DECLARE_STATIC_MESH(FLeg3TopStatics, "Ingenuity/Leg3_Top.Leg3_Top", Leg3TopStatics)

DECLARE_STATIC_MESH(FLeg4BottomStatics, "Ingenuity/Leg4_Bottom.Leg4_Bottom", Leg4BottomStatics)
DECLARE_STATIC_MESH(FLeg4BracketStatics, "Ingenuity/Leg4_Bracket.Leg4_Bracket", Leg4BracketStatics)
DECLARE_STATIC_MESH(FLeg4TopStatics, "Ingenuity/Leg4_Top.Leg4_Top", Leg4TopStatics)


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

        void addLeg(uint8_t index, UStaticMesh * bracketMesh, UStaticMesh * topMesh, UStaticMesh * bottomMesh)
        {

            vehicle.addComponent(bracketMesh, makeName("LegBracket", index, "Mesh"));
            vehicle.addComponent(topMesh,     makeName("LegTop", index, "Mesh"));
            vehicle.addComponent(bottomMesh,  makeName("LegBottom", index, "Mesh"));
        }

    public:

        void build(APawn * pawn)
        {
            // Build the frame
            vehicle.buildFull(pawn, BodyStatics.mesh.Get()); // Restore for cameras, audio
            // vehicle.build(pawn, BodyStatics.mesh.Get());

            // Add rotors
            addRotor(RotorTopStatics.mesh.Get(), .250);
            addRotor(RotorBottomStatics.mesh.Get(), .170);
            
            // Add mast, solar panel, antenna
            vehicle.addComponent(MastStatics.mesh.Get(), makeName("Mast", 1, "Mesh"));
            vehicle.addComponent(SolarPanelStatics.mesh.Get(), makeName("SolarPanel", 1, "Mesh"));
            vehicle.addComponent(AntennaStatics.mesh.Get(), makeName("Antenna", 1, "Mesh"));

            // Add legs
            addLeg(1, Leg1BracketStatics.mesh.Get(), Leg1TopStatics.mesh.Get(), Leg1BottomStatics.mesh.Get());
            addLeg(2, Leg2BracketStatics.mesh.Get(), Leg2TopStatics.mesh.Get(), Leg2BottomStatics.mesh.Get());
            addLeg(3, Leg3BracketStatics.mesh.Get(), Leg3TopStatics.mesh.Get(), Leg3BottomStatics.mesh.Get());
            addLeg(4, Leg4BracketStatics.mesh.Get(), Leg4TopStatics.mesh.Get(), Leg4BottomStatics.mesh.Get());

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

        void addCamera(Camera * camera)
        {
            vehicle.addCamera(camera);
        }

}; // class Ingenuity 
