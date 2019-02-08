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

    /**
     * Updates the flight controller with the current IMU values, returning motor values.
     * @param timestamp  input: current time in seconds
     * @param position   input: current position in meters
     * @param velocity   input: current velocity in meters per second
     * @param param quat input: current quaternion
     * @param gyro       input: current gyrometer rates in radians/sec
     * @param accel      input: current acceleromter values in Gs
     * @return motorvals motor values between 0 and 1 implementation
     */
     virtual TArray<float> update(float timestamp, FVector position, FVector velocity, FQuat quat, FVector gyro, FVector accel) = 0;


	 /**
	  * Updates the flight controller with the current IMU values, returning motor values.
	  * @param timestamp  input: current time in seconds
	  * @param position   input: current position in meters
	  * @param velocity   input: current velocity in meters per second
	  * @param vehiclePawn the VehilcePawn object that called this method
	 * @return motorvals motor values between 0 and 1 implementation
	  */
	 virtual TArray<float> update(float timestamp, FVector position, FVector velocity, class AVehiclePawn * vehiclePawn) = 0;

    /**
     *  Factory method.
     *  @return pointer to a new SimFlightController object
     */
     static SimFlightController * createSimFlightController(void);
};
