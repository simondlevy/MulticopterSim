## OpenCV for Machine Vision in MulticopterSim

After you build the simulator, the <b>Content/C++ Classes/OpenCVModule/pawns</b> folder will contain a
pawn that will run OpenCV edge detection.  Before dragging this pawn into your map and running the
simulator, you should copy the .dll file from <b>MulticopterSim/Source/OpenCVModule/ThirdParty/OpenCV/bin</b>
into <b>MulticopterSim/Binaries/Win64</b>.

The flight controller for this pawn will simple run the motors
fast enough to take off and continue flying upward.  By modifying the code in the 
[Source/OpenCVModule](https://github.com/simondlevy/MulticopterSim/blob/master/Source/OpenCVModule/) 
directory you can use machine vision to control the vehicle.
