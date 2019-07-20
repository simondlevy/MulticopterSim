% Quadcopter PID control demo
%
% Copyright(C) 2019 Simon D.Levy
%
% MIT License

% Target 
ALTITUDE_TARGET = 10;

% PID params
ALT_P = 1.0;
VEL_P = 1.0;
VEL_I = 0;
VEL_D = 0;

% initial conditions
z = 0;
zprev = 0;
tprev = 0;
dzdt = 0;
u = 0;

% Create a multicopter simulation
copter = Multicopter;

% Start the simulation
copter.start

u = 0.55;

% Loop forever
while true

    % Get vehicle state from sim
    telem = copter.getState;
    
    % Extract time, altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
    % to PID controller.
    t =  telem(1);
    z = -telem(10);
    
    fprintf('t=%f  z=%+3.3f\n', t, z)

    copter.setMotors(u*ones(1,4))

end


