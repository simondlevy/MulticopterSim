/*
 * Build script for Hackflight module
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
    private static string ARDUINO_PATH = "D:\\Users\\levys\\Documents\\Arduino\\libraries\\";

    public HackflightModule(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] 
                { "Core", "CoreUObject", "Engine", "InputCore", "Landscape" });

        PrivateDependencyModuleNames.AddRange(new string[] { "MainModule" });

        // Hackflight support ---------------------------------------------------------------

        PrivateIncludePaths.Add(ARDUINO_PATH + "Hackflight\\src");

        // OpenCV support -------------------------------------------------------------------

        /*
        string OpenCVPath =
          Path.GetFullPath(Path.Combine(ModuleDirectory, "ThirdParty")) + "\\OpenCV";

        // Add Include path
        PublicIncludePaths.AddRange(new string[] { OpenCVPath + "\\include" });

        // Add Libraries
        PublicAdditionalLibraries.Add(OpenCVPath + "\\lib\\opencv_world452.lib");
        PublicDelayLoadDLLs.Add("opencv_world452.dll");

        PublicDefinitions.Add(string.Format("WITH_OPENCV_BINDING=1"));
        PublicDefinitions.Add("_USE_OPENCV");
        */
     }
}

