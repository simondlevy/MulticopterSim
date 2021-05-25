/*
* MulticopterSimEditor.Target.cs: Edtor Target script for MulticopterSim
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/


using UnrealBuildTool;
using System.Collections.Generic;

public class MulticopterSimSocketEditorTarget : TargetRules
{
	public MulticopterSimSocketEditorTarget(TargetInfo Target) : base(Target)
	{
		Type = TargetType.Editor;

		ExtraModuleNames.AddRange( new string[] { 
                "MainModule", 
                "SocketModule"
                } );
	}
}
