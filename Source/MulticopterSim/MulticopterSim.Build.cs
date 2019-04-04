/*
 * MulticopterSimBuild.cs: Build script for MulticopterSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


using UnrealBuildTool;
using System;
using System.IO;

public class MulticopterSim : ModuleRules
{
    public MulticopterSim(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] { "Core", "CoreUObject", "Engine", "InputCore" });

        PrivateIncludePaths.Add(Target.Platform == UnrealTargetPlatform.Win64 ?
                Environment.GetEnvironmentVariable("userprofile") + "\\Documents\\Arduino\\libraries\\Hackflight\\src" :
                Environment.GetEnvironmentVariable("HOME") + "/Documents/Arduino/libraries/Hackflight/src");


        LoadOpenCV(Target);
    }

    private string ThirdPartyPath
    {
        get { return Path.GetFullPath(Path.Combine(ModuleDirectory, "../../ThirdParty/")); }
    }

    public void LoadOpenCV(ReadOnlyTargetRules Target)
    {
        // Start OpenCV linking here!
        bool isLibrarySupported = false;

        // Create OpenCV Path 
        string OpenCVPath = Path.Combine(ThirdPartyPath, "OpenCV");

        // Get Library Path 
        string LibPath = "";
        bool isdebug = Target.Configuration == UnrealTargetConfiguration.Debug && Target.bDebugBuildsActuallyUseDebugCRT;
        LibPath = Path.Combine(OpenCVPath, "lib");
        isLibrarySupported = true;

        //Add Include path 
        PublicIncludePaths.AddRange(new string[] { Path.Combine(OpenCVPath, "include") });

        // Add Library Path 
        PublicLibraryPaths.Add(LibPath);

        //Add Libraries
        PublicAdditionalLibraries.Add("opencv_world345.lib");
        PublicDelayLoadDLLs.Add("opencv_world345.dll");

        PublicDefinitions.Add(string.Format("WITH_OPENCV_BINDING={0}", isLibrarySupported ? 1 : 0));
        PublicDefinitions.Add("_USE_OPENCV");
    }
}
