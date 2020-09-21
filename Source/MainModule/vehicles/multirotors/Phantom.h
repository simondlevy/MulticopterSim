/*
 Helper class for pawns using DJI Phantom frame
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Multirotor.hpp"
#include "../MainModule/dynamics/multirotor/QuadXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Phantom/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FPropStatics, "Phantom/Prop.Prop", PropStatics)

class Phantom {

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

        QuadXAPDynamics dynamics = QuadXAPDynamics(&params);

        MultirotorVehicle vehicle = MultirotorVehicle(&dynamics);

    private:

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

    public:

        void build(APawn * pawn)
        {
            vehicle.buildFull(pawn, FrameStatics.mesh.Get(), 1.5, 0.5);

            // Add propellers
            addProp(+1, +1);
            addProp(-1, -1);
            addProp(+1, -1);
            addProp(-1, +1);

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

        void addProp(int8_t dx, int8_t dy)
        {
            vehicle.addProp(PropStatics.mesh.Get(), dx*0.12, dy*0.12, 0.17);
        }

}; // class Phantom 
