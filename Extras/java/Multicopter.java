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

/**
 * Represents a Multicopter object communicating with MulticopterSim via UDP socket calls.
 */
public class Multicopter {

    // Inner class 
    private class MulticopterThread extends Thread {

        private final int TIMEOUT = 1000;

        public void run()
        {
            _running = true;

            while (_running) {

                byte [] motorBytes = doublesToBytes(_motorVals);

                DatagramPacket motorPacket = new DatagramPacket(motorBytes, motorBytes.length, _addr, _motorPort);

                try {
                    _motorSocket.send(motorPacket);
                }
                catch (Exception e) {
                    handleException(e);
                }

                try {
                    byte [] telemetryBytes = new byte[88];
                    DatagramPacket telemetryPacket = new DatagramPacket(_telemetryBytes, telemetryBytes.length, _addr, _telemPort);
                    _telemSocket.receive(telemetryPacket);
                }
                catch (Exception e) {
                    handleException(e);
                }

                yield();
            }

            _motorSocket.close();
            _telemSocket.close();

        } // run

        public void halt()
        {
            _running = false;
        }

        public double [] getTelemetry()
        {
            return bytesToDoubles(_telemetryBytes);
        }

        // Adapted from view-source:https://stackoverflow.com/questions/2905556/how-can-i-convert-a-byte-array-into-a-double-and-back

        private byte[] doublesToBytes(double [] doubles)
        {
            int n = doubles.length;

            byte [] bytes = new byte[8*n];

            for (int i=0; i<n; ++i) {

                long l = Double.doubleToRawLongBits(doubles[i]);

                for (int j=0; j<8; ++j) {
                    bytes[i*8+j] = (byte)((l >> (j*8)) & 0xff);
                }
            }

            return bytes;
        }

        private double[] bytesToDoubles(byte [] bytes)
        {
            int n = bytes.length>>3;

            double [] doubles = new double [n];

            for (int i=0; i<n; ++i) {

                long bits = 0;

                int beg = 8 * i;

                for (int j=0; j<8; ++j) {
                    bits = (bits << 8) | (bytes[beg+8-j-1] & 0xff);
                }

                doubles[i] = Double.longBitsToDouble(bits);
            }

            return doubles;
        }

        public MulticopterThread(String host, int motorPort, int telemetryPort, int motorCount)
        {
            _motorPort = motorPort;
            _telemPort = telemetryPort;

            try {
                _addr = InetAddress.getByName(host);
                _motorSocket = new DatagramSocket();
                _telemSocket = new DatagramSocket(telemetryPort);
                _telemSocket.setSoTimeout(TIMEOUT);
            } 
            catch (Exception e) {
                handleException(e);
            }

            _motorVals = new double [motorCount];

            _running = false;

            _telemetryBytes = new byte[88];
        }

        private int _motorPort;
        private int _telemPort;

        private double [] _motorVals;

        private byte [] _telemetryBytes;

        private boolean _running;

        InetAddress _addr;

        private DatagramSocket _motorSocket;
        private DatagramSocket _telemSocket;

        private void handleException(Exception e)
        {
        }

        public void setMotors(double [] motorVals)
        {
            for (int i=0; i<motorVals.length; ++i) {
                _motorVals[i] = motorVals[i];
            }
        }

    } // MulticopterThread

    // ================================================================================

    private MulticopterThread _thread;

    /**
      * Createas a Multicopter object.
      * @param host name of host running MulticopterSim
      * @param motorPort port over which this object will send motor commands to host
      * @param telemeteryPort port over which this object will receive telemetry  from
      * @param motorCount number of motors in vehicle running in simulator on host
      */
    public Multicopter(String host, int motorPort, int telemetryPort, int motorCount)
    {
        _thread = new MulticopterThread(host, motorPort, telemetryPort, motorCount);
    }

    /**
      * Createas a Multicopter object using a default number of motors (4).
      * @param host name of host running MulticopterSim
      * @param motorPort port over which this object will send motor commands to host
      * @param telemeteryPort port over which this object will receive telemetry  from
      */
     public Multicopter(String host, int motorPort, int telemetryPort)
    {
        _thread = new MulticopterThread(host, motorPort, telemetryPort, 4);
    }

    /**
      * Createas a Multicopter object using default parameters.
      */
     public Multicopter()
    {
        _thread = new MulticopterThread("127.0.0.1", 5000, 5001, 4);
    }

    /**
      * Begins communication with simulator running on host.
      */
    public void start()
    {
        _thread.start();
    }

    /**
      * Ends communication with simulator running on host.
      */
     public void stop()
    {
        _thread.halt();
    }

    /**
      * Returns current vehicle state as an array of the form [time, gx, gy, gz, qw, qx, qy, qz, px, py, pz],
      * where g=gyro; q=quaternion; p=position.
      * @return vehicle state 
      */
     
    public double [] getState()
    {
        return _thread.getTelemetry();

    }

    /**
      * Sets motor values.
      * @param motors array of values between 0 and 1
      */
    public void setMotors(double [] motorVals)
    {
        _thread.setMotors(motorVals);
    }
}
