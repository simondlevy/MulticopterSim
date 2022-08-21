#
# Makefile for C++ / Haskell-bbased MultiSim flight controller
#
# Copyright (C) 2022 Simon D. Levy
# 
# MIT License
# 

# Change this to where you installed Hackflight
HF_DIR = /mnt/d/Users/levys/Documents/Arduino/libraries/Hackflight
#HF_DIR = $(HOME)/Documents/Arduino/libraries/Hackflight

ALL = hackflight

HF_SRC = $(HF_DIR)/src/

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

hackflight: hackflight.o 
	g++ -o hackflight hackflight.o 

hackflight.o: hackflight.cpp $(HF_SRC)/*.h $(HF_SRC)/pids/*.h
	g++ $(CFLAGS) -I$(HF_SRC) -c hackflight.cpp

run: hackflight
	./hackflight

edit:
	vim hackflight.cpp

clean:
	rm -rf $(ALL) *.o *~
