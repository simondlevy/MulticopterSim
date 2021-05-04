ALL = pack udpserver ieee754
all: $(ALL)

pack: pack.hs
	ghc pack.hs

udpserver: udpserver.hs
	ghc udpserver.hs

ieee754: ieee754.hs
	ghc ieee754.hs

run: ieee754
	./ieee754
clean:
	rm -rf $(ALL) *.hi *.o

edit:
	vim udpserver.hs

