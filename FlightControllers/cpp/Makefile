#
# Makefile for C++ / Haskell-bbased MultiSim flight controller
#
# Copyright (C) 2022 Simon D. Levy
# 
# MIT License
# 

ARDUINO_DIR = /mnt/d/Users/levys/Documents/Arduino/libraries
#ARDUINO_DIR = $(HOME)/Documents/Arduino/libraries

HF_DIR = $(ARDUINO_DIR)/Hackflight

HF_SRC = $(HF_DIR)/src/

ETL_SRC = $(ARDUINO_DIR)/etl-arduino/src/

ALL = hackflight

all: $(ALL)

CFLAGS = -Wall -std=c++11

all: $(ALL)

hackflight: hackflight.o 
	g++ -o hackflight hackflight.o 

hackflight.o: hackflight.cpp \
	$(HF_SRC)/core/*.h \
	$(HF_SRC)/core/*/*.h \
	$(HF_SRC)/core/mixers/*.h \
	$(HF_SRC)/core/mixers/fixedpitch/*.h
	g++ $(CFLAGS) -I$(HF_SRC) -I$(ETL_SRC) -c hackflight.cpp

run: hackflight
	./hackflight

edit:
	vim hackflight.cpp

clean:
	rm -rf $(ALL) *.o *~
