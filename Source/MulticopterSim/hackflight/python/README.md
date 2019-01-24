
MulticopterSim includes support for calling Python code from C++.  For an
example of how to use MulticopterSim with Python, you should do the following:

1. Un-comment the <a href="https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/MulticopterSim.Build.cs#L20"><tt>LoadPython</tt></a> 
instruction in <b>MulticopterSimBuild.cs</b>.

2. Make sure that the <a href="https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/MulticopterSim.Build.cs#L77"><tt>PythonPath</tt></a> 
variable is set appropriately in <b>MulticopterSimBuild.cs</b>.  

3. Make sure that the appropriate Python dll file file (for example, <b>python36.dll</b>) is in your
<b>C:\Windows\System32</b> folder. If it is not, you can copy it to there from the folder you specified in the 
<tt>Pythonpath</tt> variable.

4. Un-comment the <a href="https://github.com/simondlevy/MulticopterSim/blob/master/Source/MulticopterSim/python/python_class.h#L9"><tt>#define _PYTHON</tt></a> 
directive in <b>python\python_class.h</b>.  
5. Install the [Nengo neural simulator](https://www.nengo.ai/) : <tt>pip3 install nengo</tt>
6. In the <b>python</b> folder, do <tt>python3 setup.py install</tt> to make the Python script <b>nengo_picontrol.py</b> available to MulticopterSim.
7. Run the simulator and watch the vehicle's altitude being controlled by a spiking neural network!



