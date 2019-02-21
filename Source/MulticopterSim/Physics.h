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
class Physics {

protected:

	class AVehiclePawn * _vehiclePawn;

	class UStaticMeshComponent* _vehicleMesh;

	Physics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh);

public:

	/**
     * Earth's gravity
     * 
     */
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
	* Called by AVehiclePawn::Tick() on main thread; implemented by your physics model.
	* @param deltaSeconds time since last Tick()
	* @return motor values in[0,1] for simulating propeller spin and motor sound
	*/
	 virtual TArray<float> update(float deltaSeconds) = 0;

     /**
      * Called by threads as a common way for them to get current time.
      * @return current time in seconds
      */
    float getCurrentTime(void);

    /**
     * Factory method.
	 * @param vehiclePawn Pawn object for vehicle; can be queried (e.g., position, velocity) and affected (e.g., rotated)
	 * @param vehicleMesh static mesh for vehicle; can be modifed (e.g., force added)
     *  @return pointer to a new Physics object
     */
     static Physics * createPhysics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh);
};
