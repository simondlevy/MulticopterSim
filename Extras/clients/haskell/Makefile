convert: convert.hs
	ghc convert.hs

udpserver: udpserver.hs
	ghc udpserver.hs

run: convert
	./convert
clean:
	rm -rf convert udpserver *.hi *.o

edit:
	vim convert.hs

