<img src="logo.png" width=250 align="left">

## C++ flight controller for MultiSim

The flight controller in this directory uses
[Hackflight](https://github.com/simondlevy/Hackflight),
a C++ flight-control package that also runs on actual vehicles.  To try out 
this code, first install Hackflight; then edit the top line in the 
[Makefile](https://github.com/simondlevy/MultiSim/blob/master/FlightControllers/cpp/Makefile#L9-L10)to
correspond to where you installed it.  Once you've done that, type <b>make run</b> and start
the simulator as you did with the Python flight controller.
