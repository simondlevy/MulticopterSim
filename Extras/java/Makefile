#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

Takeoff.class: Takeoff.java Multicopter.class AltitudePidController.class
	javac Takeoff.java

Multicopter.class: Multicopter.java
	javac Multicopter.java

AltitudePidController.class: AltitudePidController.java
	javac AltitudePidController.java

run: Takeoff.class 
	java Takeoff

test: Takeoff.class 
	java Takeoff

plot: Takeoff.class
	java Takeoff | ../python/plotalt.py

jar: Multicopter.java
	javac Multicopter.java
	jar cvf multicopter.jar Multicopter*.class

doc:
	javadoc -d docs Multicopter.java

clean:
	rm -rf *.jar *.class docs/ *~
