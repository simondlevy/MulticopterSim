#
# run.sh Linux script for running MulticopterSim with stdout suppressed
#
# Copyright (C) 2019 Simon D. Levy
#

# Change this to where you installed UnrealEngine
UE4=$HOME/UnrealEngine

# MIT License
$UE4/Engine/Binaries/Linux/UE4Editor > /dev/null
