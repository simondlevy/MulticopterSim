/*
 * Build script for Newflight module
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


using UnrealBuildTool;
using System;
using System.IO;

public class NewflightModule : ModuleRules
{
    // Change this to agree with your Arduino libraries install location
    private static string ARDUINO_PATH = "D:\\Users\\levys\\Documents\\Arduino\\libraries\\";

    public NewflightModule(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] 
                { "Core", "CoreUObject", "Engine", "InputCore", "Landscape" });

        PrivateDependencyModuleNames.AddRange(new string[] { "MainModule" });

        // Newflight support --------------------------------------------------------------------

        PrivateIncludePaths.Add(ARDUINO_PATH + "newflight\\src");
    }
}

