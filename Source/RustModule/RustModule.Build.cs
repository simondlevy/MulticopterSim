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
    // Change this to agree with your SDL install location
    private static string SDL_PATH = "D:\\SDL2\\";

    public RustModule(ReadOnlyTargetRules Target) : base(Target)
    {
        // Avoid warning/error from SDL
        bEnableUndefinedIdentifierWarnings = false;

        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] 
                { "Core", "CoreUObject", "Engine", "InputCore", "Landscape" });

        PrivateDependencyModuleNames.AddRange(new string[] { "MainModule" });

        PrivateIncludePaths.Add(SDL_PATH + "\\include");
     }
}

