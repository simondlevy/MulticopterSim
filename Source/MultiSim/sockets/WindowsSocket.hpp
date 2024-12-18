/*
 * Windows socket support
 *
 * Copyright (C) 2023 Simon D. Levy
 *
 * MIT License
 */

#pragma once

#pragma comment(lib,"ws2_32.lib")

#define WIN32_LEAN_AND_MEAN

#undef TEXT

#include <winsock2.h>
#include <ws2tcpip.h>

#include <stdio.h>

typedef size_t recv_size_t;
typedef SOCKET socket_t;

class Socket {

    protected:

        socket_t _sock;

        char _message[200];

        bool initWinsock(void)
        {
            WSADATA wsaData;
            int iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
            if (iResult != 0) {
                sprintf_s(
                        _message,
                        "WSAStartup() failed with error: %d\n",
                        iResult);
                return false;
            }
            return true;
        }

        void cleanup(void)
        {
            WSACleanup();
        }

        void inetPton(const char * host, struct sockaddr_in & saddr_in)
        {
            WCHAR wsz[64];
            swprintf_s(wsz, L"%S", host);
            InetPton(AF_INET, wsz,   &(saddr_in.sin_addr.s_addr));
        }

        void setUdpTimeout(uint32_t msec)
        {
            setsockopt(
                    _sock,
                    SOL_SOCKET,
                    SO_RCVTIMEO,
                    (char *)
                    &msec, sizeof(msec));

        }

        bool setNonblocking(void)
        {
            ULONG nonblock = 1;

            if (ioctlsocket(_sock, FIONBIO, &nonblock) == SOCKET_ERROR) {

                return false;
            }

            return true;
        }

    public:

        void closeConnection(void)
        {
            closesocket(_sock);
        }

        char * getMessage(void)
        {
            return _message;
        }
};
