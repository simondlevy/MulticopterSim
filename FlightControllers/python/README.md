<img src="logo2.png" height=150 align="left">

## Python flight controller for MultiSim

This folder contains a Python package and example program showing how to write a flight
controller for MultiSim using Python.  This example program ignores the input
from the game controller (joystick / RC Transmitter) and uses a PID
controller to launch the vehicle to an altitude of 10 meters.  To try the
program, run the <b>launch.py</b> script, then hit the Play button in the UE5
editor.  

For use in other programs the package can be installed via the usual  ```pip3 install -e .```
(which you may want to run with ```sudo``` on Linux and OS X).
