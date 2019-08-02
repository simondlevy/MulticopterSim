# Running MulticopterSim with Nengo

The [NengoModule](https://github.com/simondlevy/MulticopterSim/tree/NengoModule) branch supports flying with a combination
of Hackflight for general flight control and an altitude-hold PID controller
using the [Nengo](https://www.nengo.ai/) neural simulation package.  To use this module, you should first install Nengo.
Then install the module in a folder called <b>FlightModule</b> in the <b>MulticopterSim/Source</b> folder.  Finally, in the
<b>MulticopterSim/Source/FlightModule/python</b> folder, run <tt>python3 setup.py
install</tt>.  The altitude-hold will kick in when you move the auxiliary
switch to its third position.
