/*
 * Build script for HackflightModule
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


using UnrealBuildTool;
using System;
using System.IO;

public class HackflightModule : ModuleRules
{
    // Change this to agree with your Arduino libraries install location
    // private static string ARDUINO_PATH = "C:\\Users\\Administrator\\Documents\\Arduino\\libraries\\";
    private static string ARDUINO_PATH = "D:\\Users\\levys\\Documents\\Arduino\\libraries\\";

    public HackflightModule(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] { "Core", "CoreUObject", "Engine", "InputCore" });

        PrivateDependencyModuleNames.AddRange(new string[] { "MainModule" });

        PrivateIncludePaths.Add(ARDUINO_PATH + "Hackflight\\src");
        PrivateIncludePaths.Add(ARDUINO_PATH + "RoboFirmwareToolkit\\src");
    }
}