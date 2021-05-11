# Controlling MulticopterSim from Matlab 
This folder contains a simple example showing how you can control MulticopterSim from a Matlab session running on
your computer or another computer.  It uses UDP sockets for sending motor values from your Matlab program 
to the simualtor and for retrieving sensor information (gyrometer, accelerometer, position) from the simulator.

To run the example, you should:

1. Build MulticopterSim as described [here](https://github.com/simondlevy/MulticopterSim).

2. Find your [Matlab preferences directory](https://www.mathworks.com/matlabcentral/answers/309984-what-is-the-default-location-of-the-matlab-preferences-directory). 
(For example, on my computer it is <b>C:\Users\owner\AppData\Roaming\MathWorks\MATLAB\R2018a</b>) Download
[this](javaclasspath.txt) file into that preferences directory, and then modify the file to replace ```owner``` with
your own user name.

3. Launch Matlab, change your working directory to <b>MulticopterSim/Extras/matlab</b>, and run the <b>takeoff.m</b> 
script.

4. Hit F5 to launch MulticopterSim, then hit the Play button in the UE4 editor.

