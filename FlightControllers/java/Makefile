#  Makefile for Java Multicopter class
#
#  Copyright(C) 2019 Simon D.Levy
#
#  This file is part of SimFlightControl.
#
#  SimFlightControl is free software: you can redistribute it and/or modify it
#  under the terms of the GNU General Public License as published by the Free
#  Software Foundation, either version 3 of the License, or (at your option)
#  any later version.
#
#  SimFlightControl is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
#  more details.
#
#  You should have received a copy of the GNU General Public License along with
#  SimFlightControl. If not, see <https://www.gnu.org/licenses/>.

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
