'''
Display Nengo PD controller in Nengo GUI

Copyright 2018 Simon D. Levy

MIT License
'''
from nengo_pid_controller import NengoPidController

controller = NengoPidController(.1, .1, in_gui=True)

with controller.model as model:
    
    pass
