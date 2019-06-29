/*
 Java Multicopter class

 Uses UDP sockets to communicate with MulticopterSim

 Copyright(C) 2019 Simon D.Levy

 MIT License
*/

import java.io.*;
import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.net.InetAddress;

class Multicopter {

    public Multicopter(String host, int motorPort, int telemetryPort)
    {
        _motorPort = motorPort;
        _telemPort = telemetryPort;

        /*
        addr = InetAddress.getByName(HOST);

        motorSocket = DatagramSocket;
        motorSocket.setReuseAddress(1);

        telemetrySocket = DatagramSocket(TELEM_PORT);
        telemetrySocket.setReuseAddress(1);
        telemetrySocket.setSoTimeout(1000);

        telemetryBytes = ByteConverter.toByteArray(zeros(1,1));
        telemetryPacket = DatagramPacket(telemetryBytes, 8);
        */
    }

    public void start()
    {
        /*
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
            
            %fprintf('%5d: %d\n', count, length(telemetry));
            count = count + 1;
            
        end
        */
    }

    public void stop()
    {
        //motorSocket.close();
    }


    private int _motorPort;
    private int _telemPort;

    // Adapted from view-source:https://stackoverflow.com/questions/2905556/how-can-i-convert-a-byte-array-into-a-double-and-back
    private static byte[] doublesToBytes(double [] vals)
    {
        int n = vals.length;

        byte [] bytes = new byte[8*n];

        for (int i=0; i<n; ++i) {

            long l = Double.doubleToRawLongBits(vals[i]);

            for (int j=0; j<8; ++j) {
                bytes[i*8+j] = (byte)((l >> (j*8)) & 0xff);
            }
        }

        return bytes;
    }

    public static void main(String [] args)
    {
        Multicopter copter = new Multicopter("127.0.0.1", 5000, 5001);
    }
}
