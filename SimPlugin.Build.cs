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
    // https://stackoverflow.com/questions/5116977/how-to-check-the-os-version-at-runtime-e-g-windows-or-linux-without-using-a-con
    private static bool IsLinux
    {
        get
        {
            int p = (int) Environment.OSVersion.Platform;
            return (p == 4) || (p == 6) || (p == 128);
        }
    }

    public FlightModule(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] { "Core", "CoreUObject", "Engine", "InputCore" });

        if (IsLinux) {
            PrivateIncludePaths.Add(Environment.GetEnvironmentVariable("HOME") + "/Documents/Arduino/libraries/Hackflight/src");
        }
        else {
            PublicDependencyModuleNames.Add("MulticopterSim");
            PrivateIncludePaths.Add(Environment.GetEnvironmentVariable("userprofile") + "\\Documents\\Arduino\\libraries\\Hackflight\\src");
        }
    }
}
