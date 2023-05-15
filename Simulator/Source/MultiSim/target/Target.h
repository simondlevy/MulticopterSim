/*
* Class declaration for target class
*
* Copyright (C) 2019 Simon D. Levy
*
* MIT License
*/

#pragma once

#include <CoreMinimal.h>
#include <GameFramework/Pawn.h>

#include "Target.generated.h"

// A macro for simplifying the declaration of static meshes
#define DECLARE_STATIC_MESH(structname, assetstr, objname)   \
    struct structname {                                             \
        ConstructorHelpers::FObjectFinderOptional<UStaticMesh> mesh;   \
        structname() : mesh(TEXT("/Game/MultiSim/Meshes/" assetstr)) { } \
    };                                                                     \
    static structname objname;

// Structures to hold static mesh initializations
DECLARE_STATIC_MESH(FFrameStatics, "Target/Frame.Frame", FrameStatics)

UCLASS(Config=Game)
class ATarget : public APawn {

    private:

        GENERATED_BODY()

    protected:

        // AActor overrides

        // virtual void BeginPlay() override;

        // virtual void Tick(float DeltaSeconds) override;

        // virtual void PostInitializeComponents() override;

        // virtual void EndPlay(const EEndPlayReason::Type EndPlayReason) override;

    public:	

        ATarget();

}; // ATarget
