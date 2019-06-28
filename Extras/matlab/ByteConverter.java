class ByteConverter {

    // Adapted from view-source:https://stackoverflow.com/questions/2905556/how-can-i-convert-a-byte-array-into-a-double-and-back
    public static byte[] toByteArray(double [] vals)
    {
        long l = Double.doubleToRawLongBits(vals[0]);

        byte [] bytes = new byte[8];

        for (int i=0; i<8; ++i) {
            bytes[i] = (byte)((l >> (i*8)) & 0xff);
        }

        return bytes;
    }
}
