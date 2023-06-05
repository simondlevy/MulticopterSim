/*
* Class declaration for DJI Phantom pawn class using UDP sockets
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include "phantom_common.hpp"

#include "PhantomLocal.generated.h"

UCLASS(Config=Game)
class APhantomLocal : public APawn {

    private:

        GENERATED_BODY()

        Camera camera;

        QuadXBFDynamics dynamics =
            QuadXBFDynamics(phantom_vparams, phantom_fparams);

        Vehicle vehicle = Vehicle(&dynamics);

    protected:

        // AActor overrides

        virtual void BeginPlay() override;

        virtual void Tick(float DeltaSeconds) override;

        virtual void PostInitializeComponents() override;

        virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        APhantomLocal();

}; // APhantomLocal
