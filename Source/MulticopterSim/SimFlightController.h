/*
* SimFlightController.h: Abstract flight-control class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#pragma once

#include "CoreMinimal.h"
#include "VehiclePawn.h"

/**
 * Abstract class for flight control
 */
class MULTICOPTERSIM_API SimFlightController {

public:

    /**
     *  Called by AVehiclePawn::BeginPlay() when Play button is pressed
     */
    virtual void start(void) { }

    /**
     *  Called by AVehiclePawn::EndPlay() when Stop button is pressed
     */
    virtual void stop(void) { }

	 virtual TArray<float> update(float deltaSeconds, class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh) = 0;

    /**
     *  Factory method.
     *  @return pointer to a new SimFlightController object
     */
     static SimFlightController * createSimFlightController(void);
};
