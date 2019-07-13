#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

Takeoff.class: Takeoff.java Multicopter.class
	javac Takeoff.java

Multicopter.class: Multicopter.class
	javac Multicopter.java

test: Takeoff.class 
	java Takeoff

jar: Multicopter.java
	javac Multicopter.java
	jar cvf multicopter.jar Multicopter*.class

doc:
	javadoc -d docs Multicopter.java

clean:
	rm -rf *.jar *.class docs/ *~
