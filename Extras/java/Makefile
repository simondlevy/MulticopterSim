#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

multicopter.jar: edu/wlu/cs/levy/Multicopter.java
	javac edu/wlu/cs/levy/Multicopter.java
	jar cvf multicopter.jar edu

test: Multicopter.class
	java Multicopter

run: Multicopter.class
	java Multicopter

doc:
	javadoc -d doc Multicopter.java

clean:
	rm -f *.jar *.class edu/wlu/cs/levy/*.class *~
