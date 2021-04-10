<a href="https://www.youtube.com/watch?v=p_jRptn6hfg"><img src="../../Extras/media/edges.png" width=500></a>

## OpenCV for Machine Vision in MulticopterSim

After you build the simulator, the <b>Content/C++ Classes/OpenCVModule/pawns</b> folder will contain a
pawn that will run OpenCV edge detection.  

The flight controller for this pawn will simply run the motors
fast enough to take off and continue flying upward.  By modifying the code in the 
[Source/OpenCVModule](https://github.com/simondlevy/MulticopterSim/blob/master/Source/OpenCVModule/) 
directory you can use machine vision to control the vehicle.
