/*
 * General-purpose socket server class
 *
 * Copyright (C) 2019 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include "TcpSocket.hpp"

#ifndef _WIN32
static void closesocket(int socket) { close(socket); }
#endif

class TcpServerSocket : public TcpSocket {

    public:

        TcpServerSocket(
                const char * host,
                const uint16_t port,
                const bool nonblock=false)
            : TcpSocket(host, port)        
        {
            // Bind socket to address
            if (bind(_sock,
                        _addressInfo->ai_addr,
                        (int)_addressInfo->ai_addrlen) == SOCKET_ERROR) {

                closesocket(_sock);
                _sock = INVALID_SOCKET;
                sprintf_s(_message, "bind() failed");
            }

            else {

                // Listen for a connection, exiting on failure
                if (listen(_sock, 1)  == -1) {
                    sprintf_s(_message, "listen() failed");
                }

                if (nonblock) {

                    if (!setNonblocking()) {
                        sprintf_s(_message, "setNonblocking() failed");
                    }
                    
                }
            }
        }

        bool acceptConnection(void)
        {
            // Accept connection, exiting on failure
            _conn = accept(_sock, (struct sockaddr *)NULL, NULL);
            if (_conn == SOCKET_ERROR) {
                sprintf_s(_message, "accept() failed");
                return false;
            }

            return true;
        }
};
