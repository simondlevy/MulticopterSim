/*
* MultiSim.Target.cs: Target script for MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

using UnrealBuildTool;
using System.Collections.Generic;

public class MultiSimTarget : TargetRules
{
	public MultiSimTarget(TargetInfo Target) : base(Target)
	{
		Type = TargetType.Game;
        DefaultBuildSettings = BuildSettingsVersion.V2;
		ExtraModuleNames.Add("MultiSim");
	}
}
