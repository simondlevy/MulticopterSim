// Fill out your copyright notice in the Description page of Project Settings.

#define WIN32_LEAN_AND_MEAN


#include "HackflightSimSocketClientWindows.h"

// https://answers.unrealengine.com/questions/27560/trouble-using-windows-includes-with-dword-int.html
#include "AllowWindowsPlatformTypes.h"
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include "HideWindowsPlatformTypes.h"

#include <stdlib.h>
#include <stdio.h>
/*
* HackflightSimSocketClientWindows.h : Windwos Support for socket client
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#include <debug.hpp>

// Need to link with Ws2_32.lib, Mswsock.lib, and Advapi32.lib
#pragma comment (lib, "Ws2_32.lib")
#pragma comment (lib, "Mswsock.lib")
#pragma comment (lib, "AdvApi32.lib")

HackflightSimSocketClient::HackflightSimSocketClient()
{
    WSADATA wsaData;
    SOCKET ConnectSocket = INVALID_SOCKET;
    struct addrinfo *result = NULL, *ptr = NULL, hints;
    int iResult;
    int recvbuflen = BUFLEN;

    // Initialize Winsock
    iResult = WSAStartup(MAKEWORD(2,2), &wsaData);
    if (iResult != 0) {
        hf::Debug::printf("WSAStartup failed with error: %d\n", iResult);
    }

    ZeroMemory( &hints, sizeof(hints) );
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_protocol = IPPROTO_TCP;

    // Resolve the server address and port
    iResult = getaddrinfo(HOST, PORT, &hints, &result);
    if ( iResult != 0 ) {
        hf::Debug::printf("getaddrinfo failed with error: %d\n", iResult);
        WSACleanup();
    }

	_result = result;

	_connected = false;
}

HackflightSimSocketClient::~HackflightSimSocketClient()
{
}

bool HackflightSimSocketClient::isConnected()
{
	return _connected;
}

void HackflightSimSocketClient::attemptConnection()
{
	SOCKET ConnectSocket = INVALID_SOCKET;

	struct addrinfo *result = (struct addrinfo *)_result;

	// Attempt to connect to an address until one succeeds
	for (struct addrinfo * ptr = result; ptr != NULL; ptr = ptr->ai_next) {

		// Create a SOCKET for connecting to server
		ConnectSocket = socket(ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol);

		hf::Debug::printf("%d %d %d", ptr->ai_family, ptr->ai_socktype, ptr->ai_protocol);

		continue;

		if (ConnectSocket == INVALID_SOCKET) {
			hf::Debug::printf("socket failed with error: %ld\n", WSAGetLastError());
			WSACleanup();
			return;
		}

		// Connect to server.
		if (connect(ConnectSocket, ptr->ai_addr, (int)ptr->ai_addrlen) == SOCKET_ERROR) {
			closesocket(ConnectSocket);
			ConnectSocket = INVALID_SOCKET;
			continue;
		}
		break;
	}

	return;

	freeaddrinfo(result);

	if (ConnectSocket == INVALID_SOCKET) {
		printf("Unable to connect to server!\n");
		WSACleanup();
		return;
	}

	_ConnectSocket = (void *)ConnectSocket;

	_connected = true;
}

/*

int __cdecl main(int argc, char **argv)
{
    // Send an initial buffer
    iResult = send( ConnectSocket, sendbuf, (int)strlen(sendbuf), 0 );
    if (iResult == SOCKET_ERROR) {
        printf("send failed with error: %d\n", WSAGetLastError());
        closesocket(ConnectSocket);
        WSACleanup();
        return 1;
    }

    printf("Bytes Sent: %ld\n", iResult);

    // shutdown the connection since no more data will be sent
    iResult = shutdown(ConnectSocket, SD_SEND);
    if (iResult == SOCKET_ERROR) {
        printf("shutdown failed with error: %d\n", WSAGetLastError());
        closesocket(ConnectSocket);
        WSACleanup();
        return 1;
    }

    // Receive until the peer closes the connection
    do {

        iResult = recv(ConnectSocket, recvbuf, recvbuflen, 0);
        if ( iResult > 0 )
            printf("Bytes received: %d\n", iResult);
        else if ( iResult == 0 )
            printf("Connection closed\n");
        else
            printf("recv failed with error: %d\n", WSAGetLastError());

    } while( iResult > 0 );

    // cleanup
    closesocket(ConnectSocket);
    WSACleanup();

    return 0;
}
*/
