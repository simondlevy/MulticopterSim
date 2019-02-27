<a href="https://www.youtube.com/watch?v=mobemDcX9ew"><img src="media/IndoorScene.png" width=800></a>

# About

MulticopterSim is a simple quadcopter flight simulator using Unreal Engine 4.  It runs on Windows and Linux.

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

I are currently developing MulticopterSim on an HP Z440 workstation with 32GB
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

## Toolchain

You will need Unreal Engine 4 (UE4). I am attempting to use the latest version, which as of the time of this
writing is UE4.21.1.  

Windows users will need Visual Studio Community (we're using the latest version, 2017).
If you don't have UE4 or Visual Studio 2017 installed, these
[instructions](https://docs.unrealengine.com/latest/INT/Programming/Development/VisualStudioSetup/#visualstudio2017users) 
will help get you started. If you've already been using C++ to develop video games with
older versions of these tools (Visual Studio 2015, UE 4.16), we recommend sticking with those, as we've found that
the differences between Visual Studio 2015 and 2017 can cause problems for UE4 if you're not careful (inability
to generate a .sln file from a .uproject file, inability to compile source code, etc.).

# Build

You should first clone the MulticopterSim repository into your <b>Documents/Unreal Projects</b> folder, creating
that folder if it doesn't already exist.

Although MulticopterSim is designed to work with any flight-control software
you like, it is set up to work with the
[Hackflight](https://github.com/simondlevy/Hackflight) software by default. So to get started, you should 
clone the [Hackflight](https://github.com/simondlevy/Hackflight) repository into your <b>Arduino/libraries</b>
folder, first creating that folder if it doesn't already exist.  (You don't need to install Arduino; this is
simply where MulticopterSim 
[looks for](https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/MulticopterSim.Build.cs#L21-L24) 
the Hackflight software.)

Now double-click on <b>MulticopterSim.uproject</b> to launch UnrealEditor; then click the Compile icon at the top. 
You can then use your favorite code editor (ours is 
<a href="https://www.vim.org">vim</a>) to edit the source code in <b>Source/MulticopterSim</b>.

## Windows

If you have any difficulty building the project on Windows, you can right-click on the <b>MulticopterSim.uproject</b> 
file and select <b>Generate Visual Studio project file</b> to generate a <b>.sln</b> file, which will
launch Visual Studio with a double-click.

## Linux

If you run into trouble building the project on Linux, you can run the <b>build.sh</b> script, which will show
you the compiling and linking of the project file-by-file.  You should
[edit](https://github.com/simondlevy/MulticopterSim/blob/master/build.sh#L8-L9)
this script to reflect where you installed UnrealEngine.

# Launch and fly!

In UnrealEditor, select one of the maps in <b>Content/Flying/Maps</b>. Click
the play button and you're ready to begin. Throttle up to fly.  You can use the
spacebar to switch between the ground camera and FPV camera.

# Support for OpenCV (Windows only)

To enable projects using machine vision, MulticopterSim includes support for the popular OpenCV package.
To use OpenCV with MulticopterSim, you should do the following:

1. Un-comment the <a href="https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/MulticopterSim.Build.cs#L20"><tt>LoadOpenCV</tt></a> 
instruction in <b>MulticopterSimBuild.cs</b>.

2. After building the game, copy the file <b>opencv_world340.dll</b> from the <b>ThirdParty/OpenCV/Libraries/Win64/</b> to <b>Binaries/Win64/</b>.

# Support for other flight-control software

MulticopterSim has a simple API for interfacing with other flight-control software: create a subclass of the 
abstract [SimFlightController](https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/SimFlightController.h#L17-L39) 
class, implementing its virtual ```init``` and ```update``` methods, as well as the ```createSimFlightController``` factory method. 
Here is default the
[subclass](https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/hackflight/SimFlightController.cpp)
that uses the [Hackflight](https://github.com/simondlevy/Hackflight) firmware.

