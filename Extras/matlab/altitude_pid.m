% Quadcopter PID control demo
%
% Don't forget to start the simulator first!
%
% Copyright(C) 2019 Simon D.Levy
%
% MIT License

%javaaddpath('multicopter.jar')

import Multicopter

copter = Multicopter;

copter.start
copter.setMotors(zeros(1,4))
pause(1)
copter.setMotors(.6*ones(1,4))
pause(3)
copter.setMotors(zeros(1,4))
copter.stop


