/*
 Helper class for pawns using rocket frame
 *
 * Copyright (C) 2020 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Multirotor.hpp"

#include "../../MainModule/dynamics/ThrustVector.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FBodyStatics,   "Rocket/Body.Body",     BodyStatics)
DECLARE_STATIC_MESH(FRotor1Statics, "Rocket/Rotor1.Rotor1", Rotor1Statics)
DECLARE_STATIC_MESH(FRotor2Statics, "Rocket/Rotor2.Rotor2", Rotor2Statics)
DECLARE_STATIC_MESH(FNozzleStatics, "Rocket/Nozzle.Nozzle", NozzleStatics)

class Rocket {

    private:

        Dynamics::vehicle_params_t vparams = {

            // Estimated
            5.E-06, // b force constatnt [F=b*w^2]
            2.E-06, // d torque constant [T=d*w^2]

            // https://www.dji.com/phantom-4/info
            1.380,  // m mass [kg]

            // Estimated
            2,      // Ix [kg*m^2] 
            2,      // Iy [kg*m^2] 
            3,      // Iz [kg*m^2] 
            38E-04, // Jr prop inertial [kg*m^2] 

            0.350,  // l arm length [m]

            15000 // maxrpm
        };

        // Affects dynamics
        static constexpr double NOZZLE_MAX_ANGLE =  45;
        static constexpr double NOZZLE_Z         =  0.15;

        // For appearance only
        static constexpr double ROTOR1_Z  =  0.60;
        static constexpr double ROTOR2_Z  =  0.70;

        // A private class to support animating the nozzle
        class NozzleVehicle : public MultirotorVehicle {

            friend class Rocket;

            UStaticMeshComponent * nozzleMeshComponent = NULL;

            NozzleVehicle(Dynamics* dynamics) 
                : MultirotorVehicle(dynamics, 4)
            {
            }

            virtual void animateActuators(void) override
            {
                MultirotorVehicle::animateActuators();

                nozzleMeshComponent->SetRelativeRotation(FRotator(-_motorvals[3]*NOZZLE_MAX_ANGLE, 0, -_motorvals[2]*NOZZLE_MAX_ANGLE));
            }
        };

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

        void addRotor(UStaticMesh* mesh, float z)
        {
            _vehicle->addProp(mesh, 0, 0, z);
        }

        float meshHeightMeters(UStaticMesh * mesh) 
        {
            FBox box = mesh->GetBoundingBox();

            return (box.Max.Z - box.Min.Z) / 100; // cm => m
        }

        NozzleVehicle * _vehicle = NULL;

    public:

        ThrustVectorDynamics * dynamics = NULL;

        void build(APawn * pawn)
        {
            // Get height of barrel for dynamics
            float barrelHeight = meshHeightMeters(BodyStatics.mesh.Get());

            // Create dynamics
            dynamics = new ThrustVectorDynamics(vparams, NOZZLE_MAX_ANGLE);

            // Create vehicle object from dynamics
            _vehicle = new NozzleVehicle(dynamics);

            // Add barrel mesh to vehicle
            _vehicle->buildFull(pawn, BodyStatics.mesh.Get());

            // Add rotors
            addRotor(Rotor1Statics.mesh.Get(), ROTOR1_Z);
            addRotor(Rotor2Statics.mesh.Get(), ROTOR2_Z);

            // Add nozzle
            _vehicle->nozzleMeshComponent = _vehicle->addComponent(NozzleStatics.mesh.Get(), FName("Nozzle"), 0, 0, NOZZLE_Z, 0);

            _flightManager = NULL;
        }

        void PostInitializeComponents()
        {
            _vehicle->PostInitializeComponents();
        }

        void BeginPlay(FFlightManager * flightManager)
        {
            _flightManager = flightManager;

            _vehicle->BeginPlay(flightManager);
        }

        void EndPlay(void)
        {
            FThreadedManager::stopThread((FThreadedManager **)&_flightManager);
        }

        void Tick(float DeltaSeconds)
        {
            _vehicle->Tick(DeltaSeconds);
        }

        void addCamera(Camera * camera)
        {
            _vehicle->addCamera(camera);
        }


}; // class Rocket 
