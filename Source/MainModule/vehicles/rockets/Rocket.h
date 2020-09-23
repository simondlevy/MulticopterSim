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

        // Estimated
        static constexpr double b = 5.E-06; // force constexpratnt [F=b*w^2]
        static constexpr double d = 2.E-06; // torque constexprant [T=d*w^2]

        // https://www.dji.com/phantom-4/info
        static constexpr double m = 1.380;  // mass [kg]

        // Estimated
        static constexpr double Ix = 2;      // [kg*m^2] 
        static constexpr double Iy = 2;      // [kg*m^2] 
        static constexpr double Iz = 3;      // [kg*m^2] 
        static constexpr double Jr = 38E-04; // prop inertial [kg*m^2] 

        static const uint16_t maxrpm = 15000; // maxrpm

        // A private class to support animating the nozzle
        class NozzleVehicle : public MultirotorVehicle {

            friend class Rocket;

            UStaticMeshComponent * nozzleMeshComponent = NULL;

            NozzleVehicle(Dynamics* dynamics) 
                : MultirotorVehicle(dynamics)
            {
            }

            virtual void animateActuators(void) override
            {
                MultirotorVehicle::animateActuators();

                nozzleMeshComponent->SetRelativeRotation(FRotator(-_motorvals[3]*90, 0, -_motorvals[2]*90));
            }
        };

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

        void addRotor(UStaticMesh* mesh, float z)
        {
            vehicle.addProp(mesh, 0, 0, z);
        }

    public:

        ThrustVectorDynamics dynamics = ThrustVectorDynamics(b, d, m, Ix, Iy, Iz, Jr, maxrpm);

        NozzleVehicle vehicle = NozzleVehicle(&dynamics);

        void build(APawn * pawn)
        {
            vehicle.buildFull(pawn, BarrelStaticss.mesh.Get(), 1.5, 0.5);

            // Add rotors
            addRotor(Rotor1Statics.mesh.Get(), 0.3);
            addRotor(Rotor2Statics.mesh.Get(), 0.4);

            // Add nozzle
            vehicle.nozzleMeshComponent = vehicle.addComponent(NozzleStatics.mesh.Get(), FName("Nozzle"), 0, 0, 0.2, 0);

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
