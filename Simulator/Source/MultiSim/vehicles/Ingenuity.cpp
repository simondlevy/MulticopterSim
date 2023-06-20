/*
* Class implementation for Ingenuity pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include "Ingenuity.h"
#include "../Thread.hpp"

AIngenuity::AIngenuity()
{
    // Build the frame, restoring for cameras, audio
    vehicle.buildFull(this, BodyStatics.mesh.Get()); 

    // Add rotors
    addRotor(RotorTopStatics.mesh.Get(), .250);
    addRotor(RotorBottomStatics.mesh.Get(), .170);

    // Add mast, solar panel, antenna
    vehicle.addComponent(MastStatics.mesh.Get(), makeMeshName("Mast"));
    vehicle.addComponent(SolarPanelStatics.mesh.Get(),
            makeMeshName("SolarPanel"), 0, 0, 0.34);
    vehicle.addComponent(AntennaStatics.mesh.Get(), makeMeshName("Antenna"));

    // Add legs
    addLeg(1, Leg1BracketStatics.mesh.Get(), Leg1TopStatics.mesh.Get(),
            Leg1BottomStatics.mesh.Get());
    addLeg(2, Leg2BracketStatics.mesh.Get(), Leg2TopStatics.mesh.Get(),
            Leg2BottomStatics.mesh.Get());
    addLeg(3, Leg3BracketStatics.mesh.Get(), Leg3TopStatics.mesh.Get(),
            Leg3BottomStatics.mesh.Get());
    addLeg(4, Leg4BracketStatics.mesh.Get(), Leg4TopStatics.mesh.Get(),
            Leg4BottomStatics.mesh.Get());
}

// Called when the game starts or when spawned
void AIngenuity::BeginPlay()
{
    vehicle.beginPlay(new FVehicleThread(&dynamics));

    Super::BeginPlay();
}

void AIngenuity::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    vehicle.endPlay();

    Super::EndPlay(EndPlayReason);
}

void AIngenuity::PostInitializeComponents()
{
    vehicle.postInitializeComponents();

    Super::PostInitializeComponents();
}

// Called automatically on main thread
void AIngenuity::Tick(float DeltaSeconds)
{
    vehicle.tick(DeltaSeconds);

    Super::Tick(DeltaSeconds);
}

void AIngenuity::addLeg(
        uint8_t index,
        UStaticMesh * bracketMesh,
        UStaticMesh * topMesh,
        UStaticMesh * bottomMesh)
{
    vehicle.addComponent(bracketMesh, makeName("LegBracket", index));
    vehicle.addComponent(topMesh,     makeName("LegTop", index));
    vehicle.addComponent(bottomMesh,  makeName("LegBottom", index));
}

void AIngenuity::addRotor(UStaticMesh* propMesh, float z)
{
    vehicle.addRotor(propMesh, 0, 0, z);
}
