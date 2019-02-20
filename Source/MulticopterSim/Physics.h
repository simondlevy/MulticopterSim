/*
* Physics.h: Abstract physics class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#pragma once

#include "CoreMinimal.h"
#include "VehiclePawn.h"

/**
 * Abstract class for vehicle physics
 */
class MULTICOPTERSIM_API Physics {

protected:

	class AVehiclePawn * _vehiclePawn;

	class UStaticMeshComponent* _vehicleMesh;

	Physics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh);

    float getCurrentTime(void);

public:


	// Earth's gravity
	static constexpr float G = 9.80665f;

    /**
     *  Called by AVehiclePawn::BeginPlay() when Play button is pressed
     */
    virtual void start(void) { }

    /**
     *  Called by AVehiclePawn::EndPlay() when Stop button is pressed
     */
    virtual void stop(void) { }

	/**
	* Implemented by your physics model.
	* @param deltaSeconds time since last Tick()
	* @return motor values in[0,1] for simulating propeller spin and motor sound
	*/
	 virtual TArray<float> update(float deltaSeconds) = 0;

    /**
     *  Factory method.
	 * @param vehiclePawn Pawn object for vehicle; can be queried (e.g., position, velocity) and affected (e.g., rotated)
	 * @param vehicleMesh static mesh for vehicle; can be modifed (e.g., force added)
     *  @return pointer to a new Physics object
     */
     static Physics * createPhysics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh);
};
