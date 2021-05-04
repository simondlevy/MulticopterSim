ALL = pack udpserver
all: $(ALL)

pack: pack.hs
	ghc pack.hs

udpserver: udpserver.hs
	ghc udpserver.hs

run: udpserver
	./udpserver
clean:
	rm -rf $(ALL) *.hi *.o

edit:
	vim udpserver.hs

