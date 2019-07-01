# Controlling MulticopterSim from Matlab

This folder contains a simple example showing how you can control MulticopterSim from a Matlab session running on
your computer or another computer.  It uses UDP sockets for sending motor values from your Matlab program 
to the simualtor and for retrieving the vehicle state (time, gyrometer, quaternion, position) from the simulator.

To run the example, you should:

1. Clone the master branch of this repostitory into your <b>Documents/Unreal
   Projects</b> folder, first creating that folder if it doesn't exist.
   
2. Clone the  [SocketModule](https://github.com/simondlevy/MulticopterSim/tree/SocketModule) branch
of the repository onto your desktop, rename it <b>FlightModule</b>, and drag the
<b>FlightModule</b> folder into the <b>MulticopterSim/Source</b> folder.

3. Find your [Matlab preferences directory](https://www.mathworks.com/matlabcentral/answers/309984-what-is-the-default-location-of-the-matlab-preferences-directory). 
For example, on my computer it is <b>C:\Users\simon\AppData\Roaming\MathWorks\MATLAB\R2018a</b>
In that preferences directory, add to or modify a file <b>javaclasspath.txt</b> to contain the line:
```C:\Users\<USERNAME>\Documents\Unreal Projects\MulticopterSim\Extras\matlab\multicopter.jar```
where ```<USERNAME>``` is of course your user name (home folder).  
