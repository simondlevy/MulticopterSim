#
# Makefile for Hackflight-based Multicopter server
#
# Copyright (C) 2022 Simon D. Levy
# 
# MIT License
# 

ALL = hackflight

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

hackflight: hackflight.o 
	g++ -o hackflight hackflight.o 

hackflight.o: hackflight.cpp ../../Source/MainModule/Dynamics.hpp
	g++ $(CFLAGS) -c hackflight.cpp

run: hackflight
	./hackflight

edit:
	vim hackflight.cpp

clean:
	rm -rf $(ALL) *.o *~
