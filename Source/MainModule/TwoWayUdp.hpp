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

        TwoWayUdp(const char * host, const short client_port, const short server_port) 
        {
            _client = new UdpClientSocket(host, client_port);
            _server = new UdpServerSocket(server_port);
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
