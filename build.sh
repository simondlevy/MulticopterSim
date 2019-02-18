# Change this to wherever you installed UnrealEngine
UE=$HOME/UnrealEngine

PROJECT="/home/simon/Documents/Unreal Projects/MulticopterSim/MulticopterSim.uproject"

$UE/Engine/Build/BatchFiles/Linux/RunMono.sh  $UE/Engine/Binaries/DotNET/UnrealBuildTool.exe Development Linux \
    -Project="$PROJECT" -TargetType=Editor -Progress -NoHotReloadFromIDE
