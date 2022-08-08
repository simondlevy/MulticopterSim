#
# Makefile for Haskell-based MultiSim flight controller
#
# Copyright (C) 2022 Simon D. Levy
# 
# MIT License
# 

ALL = takeoff
all: $(ALL)

takeoff: takeoff.hs Types.hs Multicopter.hs AltitudeController.hs
	ghc -Wall takeoff.hs

run: takeoff
	./takeoff
clean:
	rm -rf $(ALL) *.hi *.o

edit:
	vim AltitudeController.hs

