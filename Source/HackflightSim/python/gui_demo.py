'''
Display Nengo PD controller in Nengo GUI

From command line:

    nengo gui_demo.py

Copyright 2018 Simon D. Levy

MIT License
'''
from nengo_picontrol import PIController

controller = PIController(.1, .1, in_gui=True)

with controller.model as model:
    
    pass
