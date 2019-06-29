#
# Makefile for UDP socket proxy
#
# Copyright (C) 2019 Simon D. Levy
# 
# MIT License
# 

ALL = udpproxy 

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

udpproxy: udpproxy.o 
	g++ -o udpproxy udpproxy.o 

udpproxy.o: udpproxy.cpp
	g++ $(CFLAGS) -Isockets -I../../Source/MulticopterSim/dynamics -c udpproxy.cpp

test: udpproxy
	./udpproxy

run: udpproxy
	./udpproxy

edit:
	vim udpproxy.cpp

clean:
	rm -rf $(ALL) *.o *~
