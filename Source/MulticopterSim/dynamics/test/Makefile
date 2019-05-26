all: tester

tester: tester.cpp ../MultirotorDynamics.hpp
	gcc -o tester -I.. tester.cpp

test: tester
	./tester
