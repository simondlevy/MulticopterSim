#
# Makefile for simulator proxy
#
# Copyright (C) 2019 Simon D. Levy
# 
# MIT License
# 

ALL = simproxy cfproxy

all: $(ALL)

CFLAGS = -Wall -std=c++11

HFDIR = $(HOME)/Documents/Arduino/libraries/Hackflight/src/
MSDIR = ../Source/MultiSim/

all: $(ALL)

simproxy: simproxy.o 
	g++ -o simproxy simproxy.o 

simproxy.o: simproxy.cpp $(MSDIR)/Dynamics.hpp
	g++ $(CFLAGS) -c simproxy.cpp

run: simproxy
	./simproxy

cfproxy: cfproxy.o 
	g++ -o cfproxy cfproxy.o 

cfproxy.o: cfproxy.cpp $(MSDIR)/Dynamics.hpp
	g++ $(CFLAGS) -I$(HFDIR) -I$(MSDIR) -c cfproxy.cpp

cfrun: cfproxy
	./cfproxy

edit:
	vim simproxy.cpp

cfedit:
	vim cfproxy.cpp

clean:
	rm -rf $(ALL) *.o *~
