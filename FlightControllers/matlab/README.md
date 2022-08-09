<img src="logo.png" width=125 align="left">

## Matlab flight controller for MultiSim

This folder contains an example program <b>takeoff.m</b> showing how to write a flight controller for MultiSim using Matlab.
This flight controller ignores the game controller (joystick / RC Transmitter) input and uses a PID 
controller to launch the vehicle to an altitude of 10 meters.  

The <b>takeoff.m</b> program uses the Java .jar file that's part of this directory (which was build using the code in for
the [Java-based flight controller](https://github.com/simondlevy/MultiSim/tree/master/FlightControllers/java)).  So
to use this Matlab program you should edit the
[classpath.txt](https://github.com/simondlevy/MultiSim/blob/master/FlightControllers/matlab/javaclasspath.txt)
file to reflect where you installed MultiSim.  Once you've
done that, you should be able to run the <b>takeoff.m</b> program, hit the Play button in the UE4 editor, and see the
copter rise to an altitude of 10 meters.
