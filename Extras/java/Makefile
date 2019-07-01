#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

MulticopterTest.class: MulticopterTest.java multicopter.jar
	javac MulticopterTest.java

multicopter.jar: Multicopter.java
	javac Multicopter.java
	jar cvf multicopter.jar *.class

test: MulticopterTest.class multicopter.jar
	java MulticopterTest

doc:
	javadoc -d doc Multicopter.java

clean:
	rm -f *.jar *.class edu/wlu/cs/levy/*.class *~
