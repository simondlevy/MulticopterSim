%  Quadcopter PID control demo
%
%  Uses a simple altitude set-point and velocity control, without D or I
%  terms.
%
%  Copyright(C) 2019 Simon D.Levy
%
%  This file is part of SimFlightControl.
%
%  SimFlightControl is free software: you can redistribute it and/or modify it
%  under the terms of the GNU General Public License as published by the Free
%  Software Foundation, either version 3 of the License, or (at your option)
%  any later version.
%
%  SimFlightControl is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
%  or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for
%  more details.
%
%  You should have received a copy of the GNU General Public License along with
%  SimFlightControl. If not, see <https://www.gnu.org/licenses/>.

% Target 
ALTITUDE_TARGET = 10;

% PID params
ALT_P = 1.0; % Altitude set-point
VEL_P = 1.0; % Climb rate

% Create a multicopter simulation
copter = Multicopter;

% Start the simulation
copter.start

fprintf('Hit the Play button ...')

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
    
    % Extract altitude from state.
    % to PID controller.
    z = telem(10);    
    
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


