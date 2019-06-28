import java.io.*;
import java.net.DatagramSocket;
import java.net.DatagramPacket;
import java.net.InetAddress;

class FlightManager {

    private static String HOST = "127.0.0.1";
    private static short  PORT = 5000;

    public static void main(String [] args)
    {
        try {
            InetAddress addr = InetAddress.getByName(HOST);
            DatagramSocket socket = new DatagramSocket();
            //socket.setReuseAddress(true);
            byte [] bytes = new byte[32];
            DatagramPacket packet = new DatagramPacket(bytes, 32, addr, PORT);

            for (int i=0; i<100000; ++i) {
                socket.send(packet);
            }

        } catch (Exception e) {
            System.err.println(e);
        }
    }
}
