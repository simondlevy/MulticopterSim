/*
* Class declaration for target class
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

/*
#include "Target.hpp"
#include "TargetThread.hpp"

#include "../dynamics/fixedpitch/QuadXBF.hpp"

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "Phantom.generated.h"

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Phantom/Frame.Frame", FrameStatics)

UCLASS(Config=Game)
class APhantom : public APawn {

    private:

        GENERATED_BODY()

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
            15000,// maxrpm
        };

        FixedPitchDynamics::fixed_pitch_params_t fparams = {
            5.E-06, // b thrust coefficient [F=b*w^2]
            0.350   // l arm length [m]
        };

        Camera camera;

        QuadXBFDynamics dynamics = QuadXBFDynamics(vparams, fparams);

        Target vehicle = Target(&dynamics);

        void addRotor(UStaticMesh * mesh, int8_t dx, int8_t dy);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        APhantom();

}; // APhantom
*/
