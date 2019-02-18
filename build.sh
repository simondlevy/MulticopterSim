# Change this to wherever you installed UnrealEngine
UE=$HOME/UnrealEngine

ENGINE=$UE/Engine

$ENGINE/Build/BatchFiles/Linux/RunMono.sh  $ENGINE/Binaries/DotNET/UnrealBuildTool.exe Development Linux \
    -Project="/home/simon/Documents/Unreal Projects/MulticopterSim/MulticopterSim.uproject" \
    -TargetType=Editor -Progress -NoHotReloadFromIDE
