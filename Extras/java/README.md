# Controlling MulticopterSim from Java
This folder contains a simple example showing how you can control MulticopterSim from a Java program running on
your computer or another computer.  It uses UDP sockets for sending motor values from your Java program 
to the simualtor and for retrieving the vehicle state (time, gyrometer, quaternion, position) from the simulator.

To run the example, you should:

1. Clone the master branch of this repostitory into your <b>Documents/Unreal
   Projects</b> folder, first creating that folder if it doesn't exist.
   
2. Clone the  [SocketModule](https://github.com/simondlevy/MulticopterSim/tree/SocketModule) branch
of the repository onto your desktop, rename it <b>FlightModule</b>, and drag the
<b>FlightModule</b> folder into the <b>MulticopterSim/Source</b> folder.

3. Build MulticopterSim as described [here](https://github.com/simondlevy/MulticopterSim#Windows).

4. Compile the Java source using your favorite Java compiler (or Makefile or build script in this folder).

5. Run the <b>Takeoff</b> program.

6. Hit F5 to launch MulticopterSim, then hit the play button.

