ALL = takeoff
all: $(ALL)

takeoff: takeoff.hs
	ghc -Wall takeoff.hs

run: takeoff
	./takeoff
clean:
	rm -rf $(ALL) *.hi *.o

edit:
	vim Multicopter.hs

