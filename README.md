<a href="https://www.youtube.com/watch?v=mobemDcX9ew"><img src="media/IndoorScene.png" width=800></a>

# About

MulticopterSim is a simple multicopter flight simulator using Unreal Engine 4.  It runs on Windows.

I began this project using the [V-REP platform](https://github.com/simondlevy/Hackflight-VREP) but switched to
UnrealEngine after seeing the kinds of beautiful, real-time simulations that
Microsoft was able to get with its
[AirSim](https://github.com/Microsoft/AirSim) program. 

MulticopterSim differs from AirSim in a few important ways:
* MulticopterSim can be used with the same simple flight-control 
[firmware](https://github.com/simondlevy/Hackflight/tree/master/src) that we fly on our real-world 
[quadcopters](http://diydrones.com/profiles/blogs/flight-of-the-ladybug).
* MulticopterSim is tiny, using a few thousand lines of C++ code for the simulator and firmware.
* MulticopterSim focuses exclusively on multirotor firmware, whereas AirSim
  supports deep learning and different kinds of vehicles.

# Prerequisites

## Hardware

I am currently developing MulticopterSim on an HP Z440 workstation with 32GB
RAM and NVIDIA GeForce GTX 1080 Ti, running Windows 10. It may however be
possible to develop on a less &ldquo;loaded&rdquo; machine &ndash; see
[here](https://docs.unrealengine.com/latest/INT/GettingStarted/RecommendedSpecifications/)
for the minimum requirements recommended by Unreal Engine.

For a realistic flying experience, you will also likely want some sort of game
controller or R/C transmitter.  MulticopterSim currently supports the following controllers
through the [Joystick](https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/Joystick.h) class:

* PS4 controller
* XBox 360 controller
* XBox 360 controller clone
* PS3 controller clone
* Logitech Extreme Pro 3D joystick
* Logitech F310 gamepad
* FrSky Taranis TX9 RC transmitter with mini USB cable 
* Spektrum transmitter with WS1000 wireless simulator dongle
* Great Planes RealFlight Interlink

## Toolchain

You will need Unreal Engine 4 (UE4). I am attempting to use the latest version, which as of the time of this
writing is UE4.22.0.  

Windows users will need Visual Studio Community (we're using the latest version, 2017).
If you don't have UE4 or Visual Studio 2017 installed, these
[instructions](https://docs.unrealengine.com/latest/INT/Programming/Development/VisualStudioSetup/#visualstudio2017users) 
will help get you started. If you've already been using C++ to develop video games with
older versions of these tools (Visual Studio 2015, UE 4.16), we recommend sticking with those, as we've found that
the differences between Visual Studio 2015 and 2017 can cause problems for UE4 if you're not careful (inability
to generate a .sln file from a .uproject file, inability to compile source code, etc.).

# Build

Although MulticopterSim is designed to work with any flight-control software
you like, it easiest to get started with the
[Hackflight](https://github.com/simondlevy/Hackflight) software. So to get started, you should 
do the following:

1. Clone this repository (MulticopterSim) into your <b>Documents/Unreal Projects</b> folder, first
creating that folder if it doesn't exist.

2. Clone the [HackflightSim](https://github.com/simondlevy/HackflightSim) repository into 
<b>MulticopterSim/Source/MulticopterSim</b>.

3. Clone the [Hackflight](https://github.com/simondlevy/Hackflight) repository into your 
<b>DocumentsArduino/libraries</b> folder, first creating that folder if it
doesn't already exist.  (You don't need to install Arduino; this is simply
where MulticopterSim [looks
for](https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/MulticopterSim.Build.cs#L21-L24)
the Hackflight software.)

4. Right-click on the <b>MulticopterSim.uproject</b> 
file and select <b>Generate Visual Studio project file</b> to generate a <b>.sln</b> file

5. Double-click on the resulting <b>MulticopterSim.sln</b> file to launch VisualStudio.  The first time
you do this, you may have to wait a few minutes while Visual Studio parses up all of the UE4 files needed
to build the project.

6. In VisualStudio, hit the F5 key to build the project and launch UnrealEditor.

# Launch and fly!

In UnrealEditor, select one of the maps in <b>Content/Flying/Maps</b>. Click
the play button and you're ready to begin. Throttle up to fly.  You can use the
spacebar to switch between the ground camera and FPV camera.

# Support for OpenCV

To enable projects using machine vision, MulticopterSim includes support for the popular OpenCV package.
To use OpenCV with MulticopterSim, you should do the following:

1. Un-comment the <a href="https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/MulticopterSim.Build.cs#L20"><tt>LoadOpenCV</tt></a> 
instruction in <b>MulticopterSimBuild.cs</b>.

2. After building the game, copy the file <b>opencv_world340.dll</b> from the <b>ThirdParty/OpenCV/Libraries/Win64/</b> to <b>Binaries/Win64/</b>.
