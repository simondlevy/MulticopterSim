% Quadcopter PID control demo
%
% Uses a simple altitude set-point and velocity control, without D or I
% terms.
%
% Copyright(C) 2019 Simon D.Levy
%
% MIT License

% Target 
ALTITUDE_TARGET = 10;

% PID params
ALT_P = 1.0; % Altitude set-point
VEL_P = 1.0; % Climb rate

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

% Set up initial conditions
z = 0;
zprev = 0;
tprev = 0;
dzdt = 0;
u = 0;
    
% Loop until user hits stop button
while true

    % Get vehicle state from sim
    telem = copter.getState;
    
    % Extract time, altitude from state.  
    t =  telem(1);
    
    % Negative time means user hit stop button
    if t < 0 
        break 
    end
    
    % Extract altitude from state.  Altitude is in NED coordinates, so we negate it to use as input
    % to PID controller.
    z = -telem(10);    
    
    % Compute vertical climb rate as first difference of altitude over time
    if t > tprev

        % Use temporal first difference to compute vertical velocity
        dt = t - tprev;
        dzdt = (z-zprev) / dt;
        
        % Run the PID controller
        velTarget = (ALTITUDE_TARGET - z) * ALT_P;
        velError  = velTarget - dzdt;
        u = VEL_P * velError;
        
        % Constrain correction to [0,1] to represent motor value
        u = max(0, min(1, u));    
        
    end
    
    % Update for first difference
    zprev = z;
    tprev = t;    
    
    % Set all four motors based on the PID controller value
    copter.setMotors(u*ones(1,4))

end


