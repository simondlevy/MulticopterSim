<a href="https://www.youtube.com/watch?v=mobemDcX9ew"><img src="media/IndoorScene.png" height=275></a>
<a href="https://www.youtube.com/watch?v=h6107MBgQ98"><img src="media/Mars2.png" height=275></a>

# About

MulticopterSim is a simple multicopter flight simulator using Unreal Engine.  It runs on Windows, using 
sockets to communicate stick demands, vehicle state and camera images with flight-control programs
written in various languages.  This approach supports rapid prototyping of flight-control
algorithms without having to recompile the simulator itself.

# Prerequisites

## Hardware

I am currently developing MulticopterSim on an HP Z440 workstation with 32GB
RAM and NVIDIA GeForce GTX 1080 Ti. It may however be possible to develop on a
less &ldquo;loaded&rdquo; machine &ndash; see
[here](https://docs.unrealengine.com/4.27/en-US/Basics/InstallingUnrealEngine/RecommendedSpecifications/)
for the minimum requirements recommended by Unreal Engine.

For a realistic flying experience, you will also likely want some sort of game
controller or R/C transmitter.  MulticopterSim currently supports the following controllers
through the
[Joystick](https://github.com/simondlevy/MulticopterSim/blob/master/Simulator/Source/MultiSim/Joystick.h)
class:

* PS4 controller
* XBox One controller
* XBox 360 controller
* XBox 360 controller clone
* PS3 controller clone
* Logitech Extreme Pro 3D joystick
* Logitech F310 gamepad
* FrSky Taranis TX9 RC transmitter with mini USB cable 
* FrSky XSR-Sim dongle
* Spektrum WS1000 dongle
* Great Planes RealFlight Interlink

If you don't have a controller, MulticopterSim will use input from the numeric keypad on your keyboard (make sure
that NumLock is turned on!)
The key mappings are based on those used in [Microsoft Flight Simulator](https://www.flightsimbooks.com/flightsimhandbook/keyboardcontrols.php#:~:text=Microsoft%20Flight%20Simulator%20Handbook%20%20%20Control%20,%20Keypad%202%20%2043%20more%20rows%20i).

## Toolchain

You will need Unreal Engine 5 (UE5) and Visual Studio C++. I am attempting to use the latest versions, which as of the time of this
writing are UE5.2.0 and Visual Studio 2022. To install UE5 and Visual Studio, follow the directions 
[here](https://dev.epicgames.com/community/learning/tutorials/XjvJ/unreal-engine-ue-5-1-visual-studio-2022-installation-guide).

# Building

1. Clone this repository. 

2. Right-click on the <b>Simulator/MulticopterSim.uproject</b> 
file and select <b>Generate Visual Studio project file</b> to generate a <b>.sln</b> file

3. Double-click on the resulting <b>MulticopterSim.sln</b> file to launch VisualStudio.  The first time
you do this, you may have to wait a few minutes while Visual Studio parses up all of the UE5 files needed
to build the project.

4. In VisualStudio, hit the F5 key to build the project and launch UnrealEditor.

5. In UnrealEditor, select one of the maps in <b>Content Drawer/MulticopterSim/Maps</b>. Then open the
<b>Content/C++ Classes/MulticopterSim/vehicles</b> folder and drag the <b>Phantom</b> pawn
into the map. 

6. To enable interaction outside of the editor,  the first time you run the simulator you will
want to go to <b>Edit -> Editor Preferences -> Performance</b> and disable the option <b>Use Less CPU
when in Background</b>:

# Testing

Run the <b>launch.py</b> script in <b>FlightControllers/python/</b>.  It will tell you to hit the Play
button back in the simulator.  When you hit the button, the vehicle should rise quickly to an altitude of 10 meters.

# Image processing

If you have Python OpenCV installed on your computer, you can try out the simulated camera feature of MulticopterSim by 
un-commenting the
<tt><a href="https://github.com/simondlevy/MulticopterSim/blob/master/Simulator/Source/MultiSim/vehicles/Phantom.cpp#L21-L22">vehicle.addCamera()</a></tt>
line in the source code.  Running the Python launch program again, you should see a 640x480 image showing edge detection in 
OpenCV.  This feature can be glitchy the first time you try it.

# Design principles

The core of MulticopterSim is the C++ 
[VehicleThread](https://github.com/simondlevy/MulticopterSim/blob/master/Simulator/Source/MultiSim/Thread.hpp) 
class. This class provides support for running the vehicle dynamics and the PID control
regime on its own thread, after it first disables the
built-in physics in UE5.  The dynamics used are based directly on the model
presented in this [paper](https://infoscience.epfl.ch/record/97532/files/325.pdf), 
written as a standalone, header-only C++ 
[class](https://github.com/simondlevy/MulticopterSim/blob/master/Simulator/Source/MultiSim/Dynamics.hpp)
that can be easily adapted for other simulators and applications if desired.
This class also supports different frame configurations (quadcopter,
hexacopter) via virtual methods. 

The
[Camera](https://github.com/simondlevy/MulticopterSim/blob/master/Simulator/Source/MultiSim/Camera.hpp)
class can be instantiated to transmit
the images collected by a simulated gimbal-mounted camera on the vehicle, using
a library like OpenCV.  Computer-vision algorithms running in a Camera subclass can then be used
as input to the PID control running in the VehicleThread.  The following figure
illustrates this arrangement, using a traditional
[cascade-control](https://controlguru.com/the-cascade-control-architecture/)
(slow outer loop / fast inner loop) diagram:

<img src="media/Control2.png" width=800></a>

# Citing MulticopterSim

Please cite MulticopterSim as:

```
@ARTICLE{10.3389/fnbot.2020.00016,
AUTHOR={Levy, Simon D.},   
TITLE={Robustness Through Simplicity: A Minimalist Gateway to Neurorobotic Flight},      
JOURNAL={Frontiers in Neurorobotics},      
VOLUME={14},           
YEAR={2020},      
URL={https://www.frontiersin.org/articles/10.3389/fnbot.2020.00016},       
DOI={10.3389/fnbot.2020.00016},      
ISSN={1662-5218}
}
```
