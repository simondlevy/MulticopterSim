# Change this to wherever you installed UnrealEngine
UE=$HOME/UnrealEngine

$UE/Engine/Build/BatchFiles/Linux/RunMono.sh  $UE/Engine/Binaries/DotNET/UnrealBuildTool.exe Development Linux \
    -Project="/home/simon/Documents/Unreal Projects/MulticopterSim/MulticopterSim.uproject" \
    -TargetType=Editor -Progress -NoHotReloadFromIDE
