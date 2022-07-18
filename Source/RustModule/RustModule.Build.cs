/*
 * Build script for Rust module
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


using UnrealBuildTool;
using System;
using System.IO;

public class RustModule : ModuleRules
{
    public RustModule(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] 
                { "Core", "CoreUObject", "Engine", "InputCore", "Landscape" });

        PrivateDependencyModuleNames.AddRange(new string[] { "MainModule" });

     }
}

