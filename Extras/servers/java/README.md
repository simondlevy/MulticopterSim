# Controlling MulticopterSim from Java
This folder contains a simple example showing how you can control MulticopterSim from a Java program running on
your computer or another computer.  It uses UDP sockets for sending motor values from your Java program 
to the simualtor and for retrieving the vehicle state (time, gyrometer, quaternion, position) from the simulator.

To run the example, you should:

1. Build MulticopterSim as described [here](https://github.com/simondlevy/MulticopterSim).

2. Compile the Java source using your favorite Java compiler (or Makefile or build script in this folder).

3. Run the <b>Takeoff</b> program.

4. Hit the Play button in the UE4 editor

