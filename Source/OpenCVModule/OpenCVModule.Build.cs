/*
 * Build script for OpenCVModule
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


using UnrealBuildTool;
using System;
using System.IO;

public class OpenCVModule : ModuleRules
{
    public OpenCVModule(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] 
                { "Core", "CoreUObject", "Engine", "InputCore" });
    
        /*
        // Create OpenCV Path
        string OpenCVPath = Path.Combine(ThirdPartyPath, "OpenCV");

        // Get Library Path
        string LibPath = Path.Combine(OpenCVPath, "lib");

        //Add Include path
        PublicIncludePaths.AddRange(new string[] { Path.Combine(OpenCVPath, "include") });

        // Add Library Path
        PublicLibraryPaths.Add(LibPath);

        //Add Libraries
        PublicAdditionalLibraries.Add("opencv_world345.lib");
        PublicDelayLoadDLLs.Add("opencv_world345.dll");

        PublicDefinitions.Add(string.Format("WITH_OPENCV_BINDING=1"));
        PublicDefinitions.Add("_USE_OPENCV");
        */
    }

    private string ThirdPartyPath
    {
        get { return Path.GetFullPath(Path.Combine(ModuleDirectory, "../../Extras/ThirdParty/")); }
    }
}

