'''
Display Nengo PD controller in Nengo GUI

From command line:

    nengo gui_demo.py

Copyright 2018 Simon D. Levy

MIT License
'''
from nengo_pidcontrol import PIDController

controller = PIDController(.1, .1)

with controller.model as model:
    
    pass
