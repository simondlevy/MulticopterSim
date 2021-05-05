ALL = multicopter
all: $(ALL)

multicopter: multicopter.hs
	ghc -Wall multicopter.hs

run: multicopter
	./multicopter
clean:
	rm -rf $(ALL) *.hi *.o

edit:
	vim multicopter.hs

