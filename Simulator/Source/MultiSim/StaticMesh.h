/*
 * Macro declaration for associating pawn object with static mesh in UE
 * 
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

// A macro for simplifying the declaration of static meshes
#define DECLARE_STATIC_MESH(structname, assetstr, objname)   \
    struct structname {                                             \
        ConstructorHelpers::FObjectFinderOptional<UStaticMesh> mesh;   \
        structname() : mesh(TEXT("/Game/MultiSim/Meshes/" assetstr)) { } \
    };                                                                     \
    static structname objname;
