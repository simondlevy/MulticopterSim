<a href="https://www.youtube.com/watch?v=mobemDcX9ew"><img src="HackflightSim2.png" width=800></a>

# About

HackflightSim is a simple quadcopter flight simulator using the C++ 
[Hackflight](https://github.com/simondlevy/Hackflight) firmware and Unreal
Engine 4.  It runs on Windows 10.

We began this project using the [V-REP
platform](https://github.com/simondlevy/Hackflight-VREP) but switched to
UnrealEngine after seeing the kinds of beautiful, real-time simulations that
Microsoft was able to get with its
[AirSim](https://github.com/Microsoft/AirSim) program. 

HackflightSim differs from AirSim in a few important ways:
* HackfightSim uses the same simple flight-control 
[firmware](https://github.com/simondlevy/Hackflight/tree/master/src) that we fly on our real-world 
[quadcopters](http://diydrones.com/profiles/blogs/flight-of-the-ladybug).
* HackflightSim is tiny, using a few thousand lines of C++ code for the simulator and firmware.
* HackflightSim focuses exclusively on multirotor firmware, whereas AirSim
  supports deep learning and different kinds of vehicles.

# Prerequisites

## Hardware

We are currently developing HackflightSim on an HP Z440 workstation with 32GB RAM and NVIDIA GeForce GTX 1080 Ti, running Windows 10. It may however be possible to develop on a less &ldquo;loaded&rdquo;
machine &ndash; see [here](https://docs.unrealengine.com/latest/INT/GettingStarted/RecommendedSpecifications/)
for the minimum requirements recommended by Unreal Engine.

You will also need some sort of game controller. Because HackflightSim is meant to simulate flying an
actual micro quadcopter, we do not support flying by keyboard. We are currently supporting the following
controllers:
* FrSky Taranis TX9 RC transmitter with mini USB cable 
* Spektrum transmitter with WS1000 wireless simulator dongle
* PS4 controller
* XBox 360 controller clone
* PS3 controller clone
* Logitech Extreme Pro 3D joystick

## Toolchain

You will need Unreal Engine 4 (UE4). We are attempting to use the latest version, which as of the time of this
writing is UE4.18.3.  

Windows users will need Visual Studio Community (we're using the latest version, 2017).
If you don't have UE4 or Visual Studio 2017 installed, these
[instructions](https://docs.unrealengine.com/latest/INT/Programming/Development/VisualStudioSetup/#visualstudio2017users) 
will help get you started. If you've already been using C++ to develop video games with
older versions of these tools (Visual Studio 2015, UE 4.16), we recommend sticking with those, as we've found that
the differences between Visual Studio 2015 and 2017 can cause problems for UE4 if you're not careful (inability
to generate a .sln file from a .uproject file, inability to compile source code, etc.).

# Build

You should first clone the HackflightSim repository into your <b>Documents/Unreal Projects</b> folder, creating
that folder if it doesn't already exist.

In addition to cloning HackflightSim, you will need the [Hackflight](https://github.com/simondlevy/Hackflight) 
repository.  Although Hackflight is hardware-independent, it is set up as an
Arduino library, to support its primary intended use. So the most sensible way
to install it is to clone it into your <b>Documents/Arduino/libraries</b> folder,
creating that folder if it doesn't exist.

As with any UE4 C++ project, there are two ways to build and modify the HackflightSim code on Windows:
<ol>
<li> Double-click on <b>HackflightSim.uproject</b> to launch UnrealEditor; then click the Compile icon at the top. 
You can then use your favorite code editor (ours is 
<a href="https://www.vim.org">vim</a>) to edit the source code in <b>Source/HackflightSim</b>.
<li> Right-click on <b>HackflightSim.uproject</b>, and select the menu item
<b>Generate Visual Studio project files</b>.  This should create a file
<b>HackflightSim.sln</b> and some new folders.  If you don't get the new .sln
file, it's probably because UE4 cannot find a C++ compiler for the version of
Visual Studio you've installed (see this
<a href="https://docs.unrealengine.com/latest/INT/Programming/Development/VisualStudioSetup/#beforesetting-upyourue4-to-vsworkflow">discussion</a> 
for tips).
Double-clicking on the .sln file should launch Visual Studio.  The first time
you launch Visual Studio, it can take several minutes to parse up all the C++
source code for the UE4 engine.  Once you've got the project built and your
controller plugged in, hitting the <b>F5</b> key should build the project and
launch Unreal Editor. 
</ol>

# Launch and fly!

Click the play button and you're ready to begin. Throttle up to fly.  You can
use the spacebar to switch between the ground camera and FPV camera.

# Support for OpenCV

To enable projects using machine vision, HackflightSim includes support for the popular OpenCV package.
To use OpenCV with HackflightSim, you should do the following:

1. Un-comment the <a href="https://github.com/simondlevy/HackflightSim/blob/master/Source/HackflightSim/HackflightSim.Build.cs#L20"><tt>LoadOpenCV</tt></a> 
instruction in <b>HackflightSimBuild.cs</b>.

2. Make sure that the file <b>opencv_world340.dll</b> is in your <b>C:\Windows\System32</b> folder.  If it is not,
you can copy it to there from <a href="https://github.com/simondlevy/HackflightSim/tree/master/ThirdParty/OpenCV/Libraries/Win64"><b>HackflightSim\ThirdParty\OpenCV\Libraries\Win64</b></a>

3. To see a demonstration of OpenCV in action, un-comment the <a href="https://github.com/simondlevy/HackflightSim/blob/master/Source/HackflightSim/vision/VisionAlgorithm.h#L11"><tt>#define _OPENCV</tt></a> 
directive in <b>vision/VisionAlgorithm.h</b>.

# Support for Python

HackflightSim also includes support for calling Python code from C++.  For an example of how to use HackflightSim with Python, you should do the following:

1. Un-comment the <a href="https://github.com/simondlevy/HackflightSim/blob/master/Source/HackflightSim/HackflightSim.Build.cs#L20"><tt>LoadPython</tt></a> 
instruction in <b>HackflightSimBuild.cs</b>.

2. Make sure that the <a href="https://github.com/simondlevy/HackflightSim/blob/master/Source/HackflightSim/HackflightSim.Build.cs#L77"><tt>PythonPath</tt></a> 
variable is set appropriately in <b>HackflightSimBuild.cs</b>.  

3. Make sure that the appropriate Python dll file file (for example, <b>python36.dll</b>) is in your
<b>C:\Windows\System32</b> folder. If it is not, you can copy it to there from the folder you specified in the 
<tt>Pythonpath</tt> variable.

4. Un-comment the <a href="https://github.com/simondlevy/HackflightSim/blob/master/Source/HackflightSim/python/python_class.h#L9"><tt>#define _PYTHON</tt></a> 
directive in <b>python\python_class.h</b>.  
5. Install the [Nengo neural simulator](https://www.nengo.ai/) : <tt>pip3 install nengo</tt>
6. In the <b>python</b> folder, do <tt>python3 setup.py install</tt> to make the Python script <b>nengo_picontrol.py</b> available to HackflightSim.
7. Run the simulator and watch the vehicle's altitude being controlled by a spiking neural network!



