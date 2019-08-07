#
# Makefile for DVS simulator demo
#
# Copyright (C) 2019 Simon D. Levy
# 
# MIT License
# 

ALL = simdvs 

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

simdvs: simdvs.o 
	g++ -o simdvs simdvs.o -lopencv_core -lopencv_highgui

simdvs.o: simdvs.cpp 
	g++ $(CFLAGS) -c simdvs.cpp

test: simdvs
	./simdvs

run: simdvs
	./simdvs

edit:
	vim simdvs.cpp

clean:
	rm -rf $(ALL) *.o *~
