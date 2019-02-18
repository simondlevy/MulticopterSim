# Change this to where you installed UnrealEngine
UE4=$HOME/UnrealEngine

# Change this to the name of your project
PROJECT="MulticopterSim"

$UE4/Engine/Build/BatchFiles/Linux/RunMono.sh  $UE4/Engine/Binaries/DotNET/UnrealBuildTool.exe Development Linux \
    -Project="$PWD/$PROJECT.uproject" -TargetType=Editor -Progress -NoHotReloadFromIDE
