<a href="https://www.youtube.com/watch?v=mobemDcX9ew"><img src="HackflightSim2.png" width=800></a>

# About

HackflightSim is a simple quadcopter flight simulator using the C++ 
[Hackflight](https://github.com/simondlevy/Hackflight) firmware and Unreal
Engine 4.  It runs on Windows and Linux.

We began this project using the [V-REP
platform](https://github.com/simondlevy/Hackflight-VREP) but switched to
UnrealEngine after seeing the kinds of beautiful, real-time simulations that
Microsoft was able to get with its
[AirSim](https://github.com/Microsoft/AirSim) program. 

HackflightSim differs from AirSim in a few important ways:
* HackfightSim uses the same simple flight-control 
[firmware](https://github.com/simondlevy/Hackflight/tree/master/src) that we fly on our real-world 
[quadcopters](http://diydrones.com/profiles/blogs/flight-of-the-ladybug).
* HackflightSim is tiny, using a couple thousand lines of C++ code for the simulator and firmware.
* HackflightSim focuses exclusively on multirotor firmware, whereas AirSim
  supports deep learning and different kinds of vehicles.

# Prerequisites

## Hardware

We are currently developing HackflightSim on the following two platforms (both using solid-state drives):
* HP Z440 workstation with 32GB RAM and NVIDIA GeForce GTX 1080 Ti, running Windows 10
* HP Z230 workstation with 16GB RAM and NVIDIA Quadro K620 GPU running Ubuntu 16.04

It may however be possible to develop on a less &ldquo;loaded&rdquo;
machine &ndash; see [here](https://docs.unrealengine.com/latest/INT/GettingStarted/RecommendedSpecifications/)
for the minimum requirements recommended by Unreal Engine.

You will also need some sort of game controller. Because HackflightSim is meant to simulate flying an
actual micro quadcopter, we do not support flying by keyboard. We are currently supporting the following
controllers:
* FrSky Taranis TX9 RC transmitter with mini USB cable 
* Spektrum transmitter with WS1000 wireless simulator dongle
* XBox 360 controller
* PS3 controller
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

Linux users should follow these [instructions](https://wiki.unrealengine.com/Building\_On\_Linux).  If (like us) you like
to build your projects from the bash command line, follow these
[instructions](https://forums.unrealengine.com/development-discussion/c-gameplay-programming/97022-linux-how-to-compile-c-scripts-from-terminal) to add a bash-shell command allowing you to do this.

# Build

You should first clone the HackflightSim repository into your <b>Documents/Unreal Projects</b> folder, creating
that folder if it doesn't already exist.

In addition to cloning HackflightSim, you will need the [Hackflight](https://github.com/simondlevy/Hackflight) 
repository.  Although Hackflight is hardware-independent, it is set up as an
Arduino library, to support its primary intended use. So the most sensible way
to install it is to clone it into your <b>Documents/Arduino/libraries</b> folder,
creating that folder if it doesn't exist.

At this point, Linux users should edit the lines in
[HackflightSim/Source/HackflightSim/HackflightSim.Build.cs](https://github.com/simondlevy/HackflightSim/blob/master/Source/HackflightSim/HackflightSim.Build.cs#L33-L35)
that will tell the UnrealEditor4 compiler where to find your Hackflight source
code.  (On Windows, as long as the code is in 
<b>C:\Users\\<i>YOURNAME</i>\Documents\Arduino\libraries\Hackflight\src</b>, you shouldn't need to edit this file.)

## Windows

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

## Linux

Do the following in your bash shell:

<pre>
% cd ~/Documents/Unreal Projects/HackflightSim
% unrealbuild HackflightSim.uproject
</pre>

Then launch UnrealEngineEditor and open <b>HackflightSim.uproject</b>.  If you're asked about making
a copy to deal with version incompatibility, click Okay.  Once you've done that,
it can take several minutes for the UE4 Editor to build your project.

# Launch and fly!

Click the play button and you're ready to begin. Throttle up to fly.  You can
use the spacebar to switch between the ground camera and FPV camera.
