/*
 Helper class for pawns using DJI Phantom frame
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "../Multirotor.hpp"
#include "../MainModule/dynamics/QuadXAP.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Phantom/Frame.Frame", FrameStatics)
DECLARE_STATIC_MESH(FPropStatics, "Phantom/Prop.Prop", PropStatics)

class Phantom {

    private:

        // Estimated
        static constexpr float b = 5.E-06; // force constatnt [F=b*w^2]
        static constexpr float d = 2.E-06; // torque constant [T=d*w^2]

        // https://www.dji.com/phantom-4/info
        static constexpr float m = 1.380;  // mass [kg]

        // Estimated
        static constexpr float Ix = 2;      // [kg*m^2] 
        static constexpr float Iy = 2;      // [kg*m^2] 
        static constexpr float Iz = 3;      // [kg*m^2] 
        static constexpr float Jr = 38E-04; // prop inertial [kg*m^2] 

        static constexpr float l = 0.350;  // arm length [m]

        static const uint16_t maxrpm = 15000; // maxrpm


    public:

        QuadXAPDynamics dynamics = QuadXAPDynamics(b, d, m, Ix, Iy, Iz, Jr, l, maxrpm);

        MultirotorVehicle vehicle = MultirotorVehicle(&dynamics, 4);

    private:

        // Threaded worker for flight control
        FFlightManager * _flightManager = NULL;

    public:

        void build(APawn * pawn)
        {
            vehicle.buildFull(pawn, FrameStatics.mesh.Get());

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
