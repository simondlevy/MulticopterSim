#
# Makefile for simulator proxy
#
# Copyright (C) 2019 Simon D. Levy
# 
# MIT License
# 

ALL = simproxy newproxy

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

simproxy: simproxy.o 
	g++ -o simproxy simproxy.o 

simproxy.o: simproxy.cpp ../Source/MultiSim/Dynamics.hpp
	g++ $(CFLAGS) -c simproxy.cpp

run: simproxy
	./simproxy

newproxy: newproxy.o 
	g++ -o newproxy newproxy.o 

newproxy.o: newproxy.cpp ../Source/MultiSim/Dynamics.hpp
	g++ $(CFLAGS) -c newproxy.cpp

new: newproxy
	./newproxy

edit:
	vim simproxy.cpp

clean:
	rm -rf $(ALL) *.o *~
