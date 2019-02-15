// Fill out your copyright notice in the Description page of Project Settings.


#include "FlightManager.h"
#include "BuiltinPhysics.h"

FlightManager::FlightManager()
{
}

FlightManager::~FlightManager()
{
}

#ifdef _USE_HACKFLIGHT
// Factory method for Physics class
Physics * Physics::createPhysics(class AVehiclePawn * vehiclePawn, class UStaticMeshComponent* vehicleMesh)
{
    return new BuiltinPhysics(vehiclePawn, vehicleMesh);
}
#endif
