
% FlightManager script
%
% Adapted from
%
%   https://www.mathworks.com/matlabcentral/fileexchange/24525-a-simple-udp-communications-application
%
% Copyright(C) 2019 Simon D.Levy
%
% MIT License

msg = int8([1,2,3,4]);

host = '127.0.0.1';
port = 5000;

import java.io.*
import java.net.DatagramSocket
import java.net.DatagramPacket
import java.net.InetAddress

addr = InetAddress.getByName(host);
socket = DatagramSocket;
socket.setReuseAddress(1);
packet = DatagramPacket(msg, length(msg), addr, port);

for k = 1:10000
    
    try
        socket.send(packet);
    catch sendPacketError
        try
            socket.close;
        catch
            % do nothing.
        end % try
        
        error('%s.m--Failed to send UDP packet.\nJava error message follows:\n%s',mfilename,sendPacketError.message);
        
    end % try
    
    fprintf('%d\n', k)
    
end

socket.close


