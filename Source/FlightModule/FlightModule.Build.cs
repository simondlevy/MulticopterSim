/*
 * Build script for FlightModule
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


using UnrealBuildTool;
using System;
using System.IO;

public class FlightModule : ModuleRules
{
    // Change this to agree with your Hackflight install location
    private static string PATH = "D:\\Users\\levys\\Documents\\Arduino\\libraries\\Hackflight\\src";

    public FlightModule(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] { "Core", "CoreUObject", "Engine", "InputCore" });

        PrivateIncludePaths.Add(PATH);
    }
}
