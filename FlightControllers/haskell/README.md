<img src="logo.png" width=125 align="left">

## Haskell flight controller for MultiSim

This folder contains an example program showing how to write a flight controller for MultiSim using Haskell.
This flight controller ignores the game controller (joystick / RC Transmitter) input and uses a PID 
controller to launch the vehicle to an altitude of 10 meters.  If you're running Haskell from the command line
you can simple type <b>make run</b> to start the program, then hit the Play button in the UE5 editor.
