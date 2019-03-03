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
    private string release = "340";

	public MulticopterSim(ReadOnlyTargetRules Target) : base(Target)
	{
		PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

		PublicDependencyModuleNames.AddRange(new string[] { "Core", "CoreUObject", "Engine", "InputCore" });

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
        PublicIncludePaths.AddRange(new string[] { Path.Combine(OpenCVPath, "include" + release) });

        // Add Library Path 
        PublicLibraryPaths.Add(LibPath);

        //Add Libraries
        string libname = "opencv_world" + release;
        PublicAdditionalLibraries.Add(libname + ".lib");
        PublicDelayLoadDLLs.Add(libname + ".dll");

        PublicDefinitions.Add(string.Format("WITH_OPENCV_BINDING={0}", isLibrarySupported ? 1 : 0));
    }
}
