// Copyright 1998-2017 Epic Games, Inc. All Rights Reserved.

using UnrealBuildTool;
using System;

public class HackflightSim : ModuleRules
{
	public HackflightSim(ReadOnlyTargetRules Target) : base(Target)
	{
		PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

		PublicDependencyModuleNames.AddRange(new string[] { "Core", "CoreUObject", "Engine", "InputCore" });

        // Un-comment and edit one of these lines to point to your Hackflight/src library
        //PrivateIncludePaths.Add("/home/slevy/Documents/Arduino/libraries/Hackflight/src");         // Linux
        PrivateIncludePaths.Add(Environment.GetEnvironmentVariable("userprofile") + "\\Documents\\Arduino\\libraries\\Hackflight\\src"); // Windows
	}
}
