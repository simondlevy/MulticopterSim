/*
* MultiSimEditor.Target.cs: Edtor Target script for MultiSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

using UnrealBuildTool;
using System.Collections.Generic;

public class MultiSimEditorTarget : TargetRules
{
	public MultiSimEditorTarget(TargetInfo Target) : base(Target)
	{
		Type = TargetType.Editor;
        DefaultBuildSettings = BuildSettingsVersion.V2;
		ExtraModuleNames.Add("MultiSim");
	}
}
