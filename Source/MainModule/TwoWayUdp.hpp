/*
   Helper class for control using a client/server over UDP

   Copyright(C) 2019 Simon D.Levy

   MIT License
*/

#include "../../Extras/sockets/UdpServerSocket.hpp"
#include "../../Extras/sockets/UdpClientSocket.hpp"

class TwoWayUdp {

    private:

        UdpClientSocket * _client = NULL;
        UdpServerSocket * _server = NULL;

        
    public:

        TwoWayUdp(const char * host, const short port_in, const short port_out) 
        {
            _client = new UdpClientSocket(host, port_in);
            _server = new UdpServerSocket(port_out);
        }

        ~TwoWayUdp()
        {
            _client = UdpClientSocket::free(_client);
            _server = UdpServerSocket::free(_server);
        }

        void send(void * data, size_t size)
        {
            if (_client) _client->sendData(data, size);
        }

        void receive(void * data, size_t size)
        {
            if (_server) _server->receiveData(data, size);
        }
}; 
