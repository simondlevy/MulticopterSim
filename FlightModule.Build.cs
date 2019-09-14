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

        string home = Environment.GetEnvironmentVariable("HOME");

        // Windows support: home defaults to C:\Users\<yourname>; create HOME environment variable if you don't have Arduino there
        if (home == null) {
            home = Environment.GetEnvironmentVariable("userprofile");
        }

        string path = home + "/Documents/Arduino/libraries/Hackflight/src";

        // Windows support: re-format path
        if (!IsLinux) {
            path = (path.Substring(1,1) + ":" + path.Substring(2)).Replace('/', '\\');
            PublicDependencyModuleNames.Add("MainModule");
        }

        Console.WriteLine(path);

        PrivateIncludePaths.Add(path);
    }
}
