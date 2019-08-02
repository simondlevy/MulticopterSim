<img src="nengo.png" width=600>

# Running MulticopterSim with Nengo

This directory contains code to support an altitude-hold PID controller using
the [Nengo](https://www.nengo.ai/) neural simulation package.  To use this
module, you should first install Nengo.  Then install the
[SocketModule](https://github.com/simondlevy/MulticopterSim/tree/SocketModule)
branch of MulticopterSim in a folder called <b>FlightModule</b> in the
<b>MulticopterSim/Source</b> folder.  Finally, in the
<b>MulticopterSim/Source/FlightModule/nengo</b> folder, run <tt>python3
takeoff.py</tt>, and click the play button.
