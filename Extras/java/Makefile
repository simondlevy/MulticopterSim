#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

Multicopter.class : Multicopter.java
	javac Multicopter.java

test: Multicopter.class
	java Multicopter

run: Multicopter.class
	java Multicopter

edit:
	vim Multicopter.java

doc:
	javadoc -d doc Multicopter.java

clean:
	rm -f *.class *~
