#   Makefile for Java Multicopter class
#
#   Copyright(C) 2019 Simon D.Levy
#
#   MIT License

Launch.class: Launch.java Multicopter.class LaunchPidController.class
	javac Launch.java

Multicopter.class: Multicopter.java
	javac Multicopter.java

LaunchPidController.class: LaunchPidController.java
	javac LaunchPidController.java

run: Launch.class 
	java Launch

test: Launch.class 
	java Launch

plot: Launch.class
	java Launch | ../python/plotalt.py

jar: Multicopter.java
	javac Multicopter.java
	jar cvf multicopter.jar Multicopter*.class

doc:
	javadoc -d docs Multicopter.java

clean:
	rm -rf *.jar *.class docs/ *~
