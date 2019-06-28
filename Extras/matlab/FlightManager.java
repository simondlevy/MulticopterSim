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

        for (int i=0; i<8; ++i) {
            bytes[i] = (byte)((l >> (i*8)) & 0xff);
        }

        return bytes;
    }

    public static void main(String [] args)
    {
        try {
            InetAddress addr = InetAddress.getByName(HOST);
            DatagramSocket socket = new DatagramSocket();
            byte [] bytes = toByteArray(53);
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
