/*
 Helper class for pawns using rocket frame
 *
 * Copyright (C) 2020 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Multirotor.hpp"
#include "../MainModule/dynamics/ThrustVector.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FBarrelStaticss, "Rocket/Barrel.Barrel", BarrelStaticss)
DECLARE_STATIC_MESH(FRotor1Statics, "Rocket/Rotor1.Rotor1", Rotor1Statics)
DECLARE_STATIC_MESH(FRotor2Statics, "Rocket/Rotor2.Rotor2", Rotor2Statics)
DECLARE_STATIC_MESH(FNozzleStatics, "Rocket/Nozzle.Nozzle", NozzleStatics)

class Rocket {

    private:

        Dynamics::Parameters params = Dynamics::Parameters(

                // Estimated
                5.E-06, // b force constatnt [F=b*w^2]
                2.E-06, // d torque constant [T=d*w^2]

                // https://www.dji.com/phantom-4/info
                1.380,  //  m mass [kg]
                0.350,  //  l arm length [m]

                // Estimated
                2,      // Ix [kg*m^2] 
                2,      // Iy [kg*m^2] 
                3,      // Iz [kg*m^2] 
                38E-04, // Jr prop inertial [kg*m^2] 

                15000); // maxrpm

    public:

        ThrustVectorDynamics dynamics = ThrustVectorDynamics(&params);

        MultirotorVehicle vehicle = MultirotorVehicle(&dynamics);

    private:

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

        UStaticMeshComponent * _nozzleMeshComponent = NULL;

        void addRotor(UStaticMesh* mesh, float z)
        {
            vehicle.addProp(mesh, 0, 0, z);
        }

    public:

        void build(APawn * pawn)
        {
            vehicle.buildFull(pawn, BarrelStaticss.mesh.Get(), 1.5, 0.5);

            // Add rotors
            addRotor(Rotor1Statics.mesh.Get(), 0.2);
            addRotor(Rotor2Statics.mesh.Get(), 0.3);

            // Add nozzle
            _nozzleMeshComponent = vehicle.addComponent(NozzleStatics.mesh.Get(), FName("Nozzle"), 0, 0, 0, 0);

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


}; // class Rocket 
