#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

test: TestMulticopter.class 
	java TestMulticopter

TestMulticopter.class: TestMulticopter.java Multicopter.class
	javac TestMulticopter.java

Multicopter.class: Multicopter.class
	javac Multicopter.java

jar: Multicopter.java
	javac Multicopter.java
	jar cvf multicopter.jar Multicopter*.class

doc:
	javadoc -d doc Multicopter.java

clean:
	rm -f *.jar *.class *~
