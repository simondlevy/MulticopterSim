# Controlling MulticopterSim from Java
This folder contains a simple example showing how you can control MulticopterSim from a Java program running on
your computer or another computer.  It uses UDP sockets for sending motor values from your Java program 
to the simualtor and for retrieving the vehicle state (time, gyrometer, quaternion, position) from the simulator.

To run the example, you should:

1. Build MulticopterSim as described [here](https://github.com/simondlevy/MulticopterSim).

2. In UnrealEditor, select one of the maps in <b>Content/Flying/Maps</b>. Then open the
<b>Content/C++ Classes/SocketModule/pawns</b> folder and drag one of the vehicle pawns into the map. 

3. Compile the Java source using your favorite Java compiler (or Makefile or build script in this folder).

4. Run the <b>Takeoff</b> program (<tt>java Takeoff</tt>)

5. Hit the Play button in the UE4 editor
