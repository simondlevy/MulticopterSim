import java.io.*;
import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.net.InetAddress;
import java.nio.ByteBuffer;

class FlightManager {

    private static String HOST = "127.0.0.1";
    private static short  PORT = 5000;

    private static byte[] toByteArray(double d) {
        long l = Double.doubleToRawLongBits(d);
        return new byte[] {
            (byte)((l >> 0) & 0xff),
                (byte)((l >> 8) & 0xff),
                (byte)((l >> 16) & 0xff), 
                (byte)((l >> 24) & 0xff),
                (byte)((l >> 32) & 0xff),
                (byte)((l >> 40) & 0xff),
                (byte)((l >> 48) & 0xff),
                (byte)((l >> 56) & 0xff),
        };
    }

    public static void main(String [] args)
    {
        try {
            InetAddress addr = InetAddress.getByName(HOST);
            DatagramSocket socket = new DatagramSocket();
            byte [] bytes = toByteArray(99);
            DatagramPacket packet = new DatagramPacket(bytes, 8, addr, PORT);

            long prev = 0;

            while (true) {
                long time = System.currentTimeMillis();
                if (time != prev) {
                    socket.send(packet);
                    prev = time;
                }
            }

        } catch (Exception e) {
            System.err.println(e);
        }
    }
}
