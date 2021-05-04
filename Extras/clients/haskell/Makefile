ALL = pack convert udpserver
all: $(ALL)

pack: pack.hs
	ghc pack.hs

convert: convert.hs
	ghc convert.hs

udpserver: udpserver.hs
	ghc udpserver.hs

run: udpserver
	./udpserver
clean:
	rm -rf $(ALL) *.hi *.o

edit:
	vim udpserver.hs

