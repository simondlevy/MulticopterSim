/*
* Class declaration for pawn class using Hackflight flight manager
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <hf_mixers/thrustvec.hpp>

#include "../../MainModule/vehicles/Rocket.hpp"

#include "../FlightManager.hpp"

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "RocketPawn.generated.h"

UCLASS(Config=Game)
class ARocketPawn : public APawn {

    private:

        GENERATED_BODY()

        // Helper class
        Rocket _rocket;

        FHackflightFlightManager * _flightManager = NULL;

        SimRotaryMotor _rotor1;
        SimRotaryMotor _rotor2;
        SimServoMotor _servo1;
        SimServoMotor _servo2;

        hf::MixerThrustVector _mixer = hf::MixerThrustVector(&_rotor1, &_rotor2, &_servo1, &_servo2);

        SimMotor * _motors[4] = {&_rotor1, &_rotor2, &_servo1, &_servo2};

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        ARocketPawn();

}; // ARocketPawn
