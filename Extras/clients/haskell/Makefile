ALL = udpserver
all: $(ALL)

udpserver: udpserver.hs
	ghc udpserver.hs

run: udpserver
	./udpserver
clean:
	rm -rf $(ALL) *.hi *.o

edit:
	vim udpserver.hs

