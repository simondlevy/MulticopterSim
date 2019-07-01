#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

TestMulticopter.class: TestMulticopter.java Multicopter.class
	javac TestMulticopter.java

Multicopter.class: Multicopter.class
	javac Multicopter.java

test: TestMulticopter.class 
	java TestMulticopter

jar: Multicopter.java
	javac Multicopter.java
	jar cvf multicopter.jar Multicopter*.class

doc:
	javadoc -d docs Multicopter.java

clean:
	rm -rf *.jar *.class docs/ *~
