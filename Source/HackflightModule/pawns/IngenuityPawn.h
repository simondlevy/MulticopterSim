/*
* Class declaration for Ingenuity pawn class using Hackflight flight manager
*
* Copyright (C) 2021 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <mixers/coaxial.hpp>

#include <rft_motors/mockrotary.hpp>
#include <rft_motors/mockservo.hpp>

#include "../../MainModule/vehicles/Ingenuity.hpp"

#include "../HackflightFlightManager.hpp"

#include "CoreMinimal.h"
#include "GameFramework/Pawn.h"

#include "IngenuityPawn.generated.h"

UCLASS(Config=Game)
class HACKFLIGHTMODULE_API AIngenuityPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Ingenuity _ingenuity;

        FHackflightFlightManager * _flightManager = NULL;

        rft::MockRotaryMotor _rotor1;
        rft::MockRotaryMotor _rotor2;
        rft::MockServoMotor _collective;
        rft::MockServoMotor _cyclicRoll;
        rft::MockServoMotor _cyclicPitch;

        hf::CoaxialMixer  _mixer = hf::CoaxialMixer(&_rotor1, &_rotor2,
                &_collective, &_cyclicRoll, &_cyclicPitch);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

        // virtual void NotifyHit(...) override

    public:	

        AIngenuityPawn();

}; // IngenuityPawn
