/*
* SimFlightController.h: Abstract flight-control class for MulticopterSim
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/


#pragma once

#include "CoreMinimal.h"

/**
 * 
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
     * Updates the flight controller with the current quaternion and gyrometer,
     * outputting motor values.
     * @param timestamp current time in seconds
     * @position current position in meters
     * @velocity current velocity in meters per second
     * @param quat current quaternion
     * @param gyro current gyrometer rates in radians/sec
     * @param accel current acceleromter values in Gs
     * @return motorvals motor values between 0 and 1 output by your implementation
     */
     virtual TArray<float> update(float timestamp, FVector position, FVector velocity, FQuat quat, FVector gyro, FVector accel) = 0;

    /**
     *  Performs any necessary shutdown for your subclass implementation.
     */
    virtual void halt(void) { }

    /**
     *  Factory method.
     */
     static SimFlightController * createSimFlightController(void);
};
