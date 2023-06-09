/*
 * Linux socket support
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#include <sys/types.h>
#include <sys/socket.h>
#include <fcntl.h>
#include <netdb.h>
#include <unistd.h>
#include <arpa/inet.h>

// For Windows compatibility
#define sprintf_s sprintf

typedef int socket_t;
typedef ssize_t recv_size_t;

static const int INVALID_SOCKET = -1;
static const int SOCKET_ERROR   = -1;

#include <stdio.h>

class Socket {

    protected:

        int _sock;

        char _message[200];

        bool initWinsock(void)
        {
            return true;
        }

        void cleanup(void)
        {
        }

        void inetPton(const char * host, struct sockaddr_in & saddr_in)
        {
            inet_pton(AF_INET, host, &(saddr_in.sin_addr));
        }

        void setTcpTimeout(uint32_t msec)
        {
            struct timeval timeout;
            timeout.tv_sec = msec / 1000;
            timeout.tv_usec = (msec * 1000) % 1000000;
            setsockopt(
                    _sock,
                    SOL_SOCKET,
                    SO_RCVTIMEO,
                    &timeout,
                    sizeof(timeout));
        }

        void setUdpTimeout(uint32_t msec)
        {
            struct timeval timeout;
            timeout.tv_sec = msec / 1000;
            timeout.tv_usec = (msec * 1000) % 1000000;
            setsockopt(
                    _sock, SOL_SOCKET, SO_RCVTIMEO, &timeout, sizeof(timeout));
        }

        bool setNonblocking(void)
        {
             auto flags = fcntl(_sock, F_GETFL);

             if (flags == -1) {
                 return false;
             }

            return fcntl(_sock, F_SETFL, flags | O_NONBLOCK) != -1;
        }

    public:

        void closeConnection(void)
        {
            close(_sock);
        }

        char * getMessage(void)
        {
            return _message;
        }
};
