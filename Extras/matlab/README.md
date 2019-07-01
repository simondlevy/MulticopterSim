# Controlling MulticopterSim from Matlab

This folder contains a simple example showing you can control MulticopterSim from a Matlab session running on
your computer or another computer.  It uses UDP sockets for sending motor values from your Matlab program 
to the simualtor and for retrieving the vehicle state (time, gyrometer, quaternion, position) from the simulator.

To run the example, you should:

1. Clone the master branch of this repostitory into your <b>Documents/Unreal
   Projects</b> folder, first creating that folder if it doesn't exist.
   
2. Clone the  [SocketModule](https://github.com/simondlevy/MulticopterSim/tree/SocketModule) branch
of the repository onto your desktop, rename it <b>FlightModule</b>, and drag the
<b>FlightModule</b> folder into the <b>MulticopterSim/Source</b> folder.

3. In your Matlab preferences directory, add to or modify a file <b>javaclasspath.txt</b> to contain the line
```C:\Users\<USERNAME>\Documents\Unreal Projects\MulticopterSim\Extras\matlab\multicopter.jar```
