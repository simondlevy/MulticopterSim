
% FlightManager script
%
% Adapted from
%
%   https://www.mathworks.com/matlabcentral/fileexchange/24525-a-simple-udp-communications-application
%
% Copyright(C) 2019 Simon D.Levy
%
% MIT License

HOST = '127.0.0.1';
MOTOR_PORT = 5000;
TELEM_PORT = 5001;

import java.io.*
import java.net.DatagramSocket
import java.net.DatagramPacket
import java.net.InetAddress

import ByteConverter

addr = InetAddress.getByName(HOST);

motorSocket = DatagramSocket;
motorSocket.setReuseAddress(1);

telemetrySocket = DatagramSocket(TELEM_PORT);
telemetrySocket.setReuseAddress(1);
telemetrySocket.setSoTimeout(1000);

telemetryBytes = ByteConverter.toByteArray(zeros(1,1));
telemetryPacket = DatagramPacket(telemetryBytes, 8);

count = 0;

while true
   
    motorBytes = ByteConverter.toByteArray(0.6*ones(1,4));
    motorPacket = DatagramPacket(motorBytes, length(motorBytes), addr, MOTOR_PORT);
    
    try
        motorSocket.send(motorPacket);
    catch sendPacketError
        try
            motorSocket.close;
        catch
            % do nothing.
        end % try
        
        error('%s.m--Failed to send UDP packet.\nJava error message follows:\n%s',mfilename,sendPacketError.message);
        
    end % try    
    
    try
        telemetrySocket.receive(telemetryPacket);
        
    catch receivePacketError
        try
            telemetrySocket.close;
        catch
        end % try
    
        break
    
    end % try   
    
    telemetry = telemetryPacket.getData;
    
    fprintf('%5d: %d\n', count, length(telemetry));
    count = count + 1;
        

    
end

motorSocket.close


