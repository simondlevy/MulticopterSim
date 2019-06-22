#
# Makefile for Linux joystick example
#
# Copyright (C) 2019 Simon D. Levy
# 
# MIT License
# 

ALL = joytest 

JOYDIR = ../../Source/MulticopterSim/joystick

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

joytest: joytest.o 
	g++ -o joytest joytest.o 

joytest.o: joytest.cpp 
	g++ -c $(CFLAGS) -I$(JOYDIR) joytest.cpp 

clean:
	rm -rf $(ALL) *.o *~
