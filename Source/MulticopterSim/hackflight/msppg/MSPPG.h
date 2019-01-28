// AUTO-GENERATED CODE: DO NOT EDIT!!!\n\n'

static const int MAXBUF = 256;

typedef unsigned char byte;

class MSP_Message {

    friend class MSP_Parser;

    protected:

        MSP_Message() { }
        byte bytes[MAXBUF];
        int pos;
        int len;

    public:

        byte start();
        bool hasNext();
        byte getNext();

};

class MSP_Parser {

    private:

        int state;
        byte message_direction;
        byte message_id;
        byte message_length_expected;
        byte message_length_received;
        byte message_buffer[MAXBUF];
        byte message_checksum;

    public:

        MSP_Parser();

        void parse(byte b);


        static MSP_Message serialize_RC_NORMAL(float c1, float c2, float c3, float c4, float c5, float c6, float c7, float c8);

        static MSP_Message serialize_RC_NORMAL_Request();

        void set_RC_NORMAL_Handler(class RC_NORMAL_Handler * handler);

        static MSP_Message serialize_ATTITUDE_RADIANS(float roll, float pitch, float yaw);

        static MSP_Message serialize_ATTITUDE_RADIANS_Request();

        void set_ATTITUDE_RADIANS_Handler(class ATTITUDE_RADIANS_Handler * handler);

        static MSP_Message serialize_SET_MOTOR_NORMAL(float m1, float m2, float m3, float m4);

        static MSP_Message serialize_SET_ARMED(byte flag);

    private:

        class RC_NORMAL_Handler * handlerForRC_NORMAL;

        class ATTITUDE_RADIANS_Handler * handlerForATTITUDE_RADIANS;

};


class RC_NORMAL_Handler {

    public:

        RC_NORMAL_Handler() {}

        virtual void handle_RC_NORMAL(float c1, float c2, float c3, float c4, float c5, float c6, float c7, float c8){ }

};



class ATTITUDE_RADIANS_Handler {

    public:

        ATTITUDE_RADIANS_Handler() {}

        virtual void handle_ATTITUDE_RADIANS(float roll, float pitch, float yaw){ }

};

