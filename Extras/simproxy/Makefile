#
# Makefile for simulator proxy
#
# Copyright (C) 2019 Simon D. Levy
# 
# MIT License
# 

ALL = simproxy 

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

simproxy: simproxy.o 
	g++ -o simproxy simproxy.o 

simproxy.o: simproxy.cpp ../../Source/MainModule/dynamics/MultirotorDynamics.hpp
	g++ $(CFLAGS) -Isockets -I../../Source/MainModule -c simproxy.cpp

test: simproxy
	./simproxy

run: simproxy
	./simproxy

edit:
	vim simproxy.cpp

clean:
	rm -rf $(ALL) *.o *~
