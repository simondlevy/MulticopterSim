<img src="image.png" width=600>

# Running MulticopterSim with Nengo

This directory contains code to support an altitude-hold PID controller using
the [Nengo](https://www.nengo.ai/) neural simulation package.  To use this
module, you should take the following steps:

1. Install Nengo.

2. Follow the [directions](https://github.com/simondlevy/MulticopterSim#windows) for building MulticopterSim
and choosing a vehicle.

3. Follow the [directions](https://github.com/simondlevy/MulticopterSim/tree/master/Extras/python) 
in <b>MulticopterSim/Extras/python</b> for installing the <b>multicopter_sim</b> Python library
and using a socket-based pawn.

4. In <b>MulticopterSim/Extras/nengo</b> folder, run <tt>python3 takeoff.py</tt>, and click the play button.

## GUI version

To control the simulator from the Nengo GUI, you will need to clone and install the
[hooks](https://github.com/nengo/nengo-gui/tree/hooks)
branch of NengoGUI:

```
% git clone -b hooks https://github.com/nengo/nengo-gui
% cd nengo-gui
% python3 setup.py install
```

(On Linux you may need to issue that last command with <tt>sudo</tt>.)

Then in <b>MulticopterSim/Extras/nengo</b> folder, run <tt>nengo gui_takeoff.py</tt>, click the play button in the
Nengo GUI, and then the play button in UE4 Editor.

