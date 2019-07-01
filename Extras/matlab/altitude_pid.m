% Quadcopter PID control demo
%
% Don't forget to start the simulator first!
%
% Copyright(C) 2019 Simon D.Levy
%
% MIT License

javaaddpath('mulitcopter.jar')

import Multicopter

copter = Multicopter;
copter.start;
copter.setMotors([.6,.6,.6,.6])


