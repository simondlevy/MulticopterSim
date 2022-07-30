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
        // Avoid warning/error from SDL
        bEnableUndefinedIdentifierWarnings = false;

        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] 
                { "Core", "CoreUObject", "Engine", "InputCore", "Landscape" });

        PrivateDependencyModuleNames.AddRange(new string[] { "MainModule" });

        string SDLPath =
          Path.GetFullPath(Path.Combine(ModuleDirectory, "ThirdParty")) + "\\SDL2";

        // Add Include path
        PublicIncludePaths.AddRange(new string[] { SDLPath + "\\include" });

        // Add Libraries
        PublicAdditionalLibraries.Add(SDLPath + "\\lib\\x64\\SDL2.lib");
        PublicDelayLoadDLLs.Add("SDL2.dll");

     }
}

