#
# clean.sh Linux cleanup script for MulticopterSim
#
# Copyright (C) 2019 Simon D. Levy
#
# MIT License

rm -rf WindowsNoEditor *.txt Makefile *.sln *.pri *.kdev4 *.pro *.*workspace .vs/ Build/ DerivedDataCache/ Intermediate/ Saved/
rf -f Binaries/Win64/MulticopterSimEditor.target Binaries/Win64/UE4*
