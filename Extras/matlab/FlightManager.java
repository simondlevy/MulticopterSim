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

        byte [] bytes = new byte[8];

        bytes[0] = (byte)((l >> 0) & 0xff);
        bytes[1] = (byte)((l >> 8) & 0xff);
        bytes[2] = (byte)((l >> 16) & 0xff); 
        bytes[3] = (byte)((l >> 24) & 0xff);
        bytes[4] = (byte)((l >> 32) & 0xff);
        bytes[5] = (byte)((l >> 40) & 0xff);
        bytes[6] = (byte)((l >> 48) & 0xff);
        bytes[7] = (byte)((l >> 56) & 0xff);

        return bytes;
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
