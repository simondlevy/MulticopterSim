% Multicopter test script
%
% Copyright(C) 2019 Simon D.Levy
%
% MIT License

javaaddpath('../java')

import Multicopter

copter = Multicopter;
copter.start;
copter.setMotors([.6,.6,.6,.6])


