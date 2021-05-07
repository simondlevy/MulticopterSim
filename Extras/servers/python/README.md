# Controlling MulticopterSim from Python
This folder contains a simple example showing how you can control MulticopterSim from a Python program running on
your computer or another computer.  It uses UDP sockets for sending motor values from your Matlab program 
to the simualtor and for retrieving the vehicle state (time, gyrometer, quaternion, position) from the simulator.

To run the example, you should:

1. Build MulticopterSim as described [here](https://github.com/simondlevy/MulticopterSim#Windows).

2. In UnrealEditor, select one of the maps in <b>Content/Flying/Maps</b>. Then open the
<b>Content/C++ Classes/SocketModule/pawns</b> folder and drag one of the vehicle pawns into the map. 

3. Run the <b>takeoff.py</b> script

4. Hit F5 to launch MulticopterSim, then hit the play button.

If you want to use the Multicopter class in other projects, you can install in the usual way:

```
python3 setup.py install
```

Linux users may have to run this command with <tt>sudo</tt>.

