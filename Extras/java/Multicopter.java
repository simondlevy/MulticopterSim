/*
   Java Multicopter class

   Uses UDP sockets to communicate with MulticopterSim

   Copyright(C) 2019 Simon D.Levy

   MIT License
 */

import java.lang.Thread;
import java.io.*;
import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.net.InetAddress;

class Multicopter extends Thread {

    private final int TIMEOUT = 1000;

    public Multicopter(String host, int motorPort, int telemetryPort, int motorCount)
    {
        construct(host, motorPort, telemetryPort, motorCount);
    }

    public Multicopter(String host, int motorPort, int telemetryPort)
    {
        construct(host, motorPort, telemetryPort, 4);
    }

    public void run()
    {
        _running = true;

        int count = 0;

        while (_running) {

            getMotors();

            byte [] motorBytes = doublesToBytes(_motorVals);

            DatagramPacket motorPacket = new DatagramPacket(motorBytes, motorBytes.length, _addr, _motorPort);

            try {
                _motorSocket.send(motorPacket);
            }
            catch (Exception e) {
                handleException(e);
            }

            byte [] telemetryBytes = new byte[8];
            DatagramPacket telemetryPacket = new DatagramPacket(telemetryBytes, telemetryBytes.length, _addr, _telemPort);

            try {
                _telemSocket.receive(telemetryPacket);
            }
            catch (Exception e) {
            }

            System.out.printf("%d: ", count);
            for (int i=0; i<8; ++i) {
                System.out.printf("%X ", telemetryBytes[i]);
            }
            System.out.println();
            count++;

            yield();
        }

        _motorSocket.close();
        _telemSocket.close();

    } // run

    private void getMotors()
    {
        for (int i=0; i<_motorVals.length; ++i) {
            _motorVals[i] = 0.6;
        }
    }

    public void halt()
    {
        _running = false;
    }

    private void construct(String host, int motorPort, int telemetryPort, int motorCount)
    {
        _motorPort = motorPort;
        _telemPort = telemetryPort;

        try {
            _addr = InetAddress.getByName(host);
            _motorSocket = new DatagramSocket();
            _motorSocket.setReuseAddress(true);
            _telemSocket = new DatagramSocket(telemetryPort);
            _telemSocket.setReuseAddress(true);
            _telemSocket.setSoTimeout(TIMEOUT);
        } 
        catch (Exception e) {
            handleException(e);
        }

        _motorVals = new double [motorCount];

        _running = false;
    }


    private int _motorPort;
    private int _telemPort;

    private double [] _motorVals;

    private boolean _running;

    InetAddress _addr;

    private DatagramSocket _motorSocket;
    private DatagramSocket _telemSocket;

    private static void handleException(Exception e)
    {
    }

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

        copter.start();

        try {
            Thread.sleep(10000);
        }
        catch (Exception e) {
        }

        copter.halt();
    }
}
