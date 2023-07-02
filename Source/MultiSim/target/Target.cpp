/*
* Class implementation for target pawn in MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
 */

#include "Target.h"


ATarget::ATarget() 
{
}

// Called when the game starts or when spawned
void ATarget::BeginPlay()
{
    Super::BeginPlay();
}

void ATarget::EndPlay(const EEndPlayReason::Type EndPlayReason)
{
    Super::EndPlay(EndPlayReason);
}

void ATarget::PostInitializeComponents()
{
    Super::PostInitializeComponents();
}

// Called automatically on main thread
void ATarget::Tick(float DeltaSeconds)
{
    Super::Tick(DeltaSeconds);
}
