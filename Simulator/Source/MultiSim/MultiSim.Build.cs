/*
 * Build script for MultiSim
 *
 * Copyright (C) 2018 Simon D. Levy
 *
 * MIT License
 */


using UnrealBuildTool;
using System;
using System.IO;

public class MultiSim : ModuleRules
{
    public MultiSim(ReadOnlyTargetRules Target) : base(Target)
    {
        PCHUsage = PCHUsageMode.UseExplicitOrSharedPCHs;

        PublicDependencyModuleNames.AddRange(new string[] 
                { "Core", "CoreUObject", "Engine", "InputCore", "Landscape" });

        // Supports using Hackflight core code in local thread
        PrivateIncludePaths.Add( "../../../../Arduino/libraries/Hackflight/src/core");
    }
}
