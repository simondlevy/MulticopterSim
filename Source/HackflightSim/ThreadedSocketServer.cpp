/*

Windows implementation of threaded socket server

Copyright Simon D. Levy 2018

MIT License
*/

#include "ThreadedSocketServer.h"

#define WIN32_LEAN_AND_MEAN

#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {

	SOCKET ListenSocket;
	SOCKET ClientSocket;

    char errmsg[100];

} socket_info_t;

DWORD WINAPI threadfunc(LPVOID lpParameter)
{
	socket_info_t * sockinfo = (socket_info_t *)lpParameter;

	int iResult = listen(sockinfo->ListenSocket, SOMAXCONN);
	if (iResult == SOCKET_ERROR) {
		sprintf_s(sockinfo->errmsg, "listen failed with error: %d\n", WSAGetLastError());
		closesocket(sockinfo->ListenSocket);
		WSACleanup();
		return false;
	}

	// Accept a client socket
	sockinfo->ClientSocket = accept(sockinfo->ListenSocket, NULL, NULL);
	if (sockinfo->ClientSocket == INVALID_SOCKET) {
		sprintf_s(sockinfo->errmsg, "accept failed with error: %d\n", WSAGetLastError());
		closesocket(sockinfo->ListenSocket);
		WSACleanup();
		return false;
	}

    // Make client socket non-blocking
	unsigned long iMode = 1; // non-blocking
	iResult = ioctlsocket(sockinfo->ClientSocket, FIONBIO, &iMode);
	if (iResult != NO_ERROR) {
		sprintf_s(sockinfo->errmsg, "ioctlsocket failed with error: %d\n", iResult);
	}

	// No longer need server socket
	closesocket(sockinfo->ListenSocket);

	return 0;
}


ThreadedSocketServer::ThreadedSocketServer(int port, const char * host)
{
    strcpy_s(_host, host);
    _port = port;

	socket_info_t * sockinfo = new socket_info_t;
	sockinfo->ListenSocket = INVALID_SOCKET;
	sockinfo->ClientSocket = INVALID_SOCKET;
	_sockinfo = (void *)sockinfo;

}

bool ThreadedSocketServer::start(void)
{
	socket_info_t * sockinfo = (socket_info_t *)_sockinfo;

	// Initialize Winsock
	WSADATA wsaData;
	int iResult = WSAStartup(MAKEWORD(2, 2), &wsaData);
	if (iResult != 0) {
		sprintf_s(sockinfo->errmsg, "WSAStartup failed with error: %d\n", iResult);
		return false;
	}

	// Create a socket to listen for a client
	sockinfo->ListenSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
	if (sockinfo->ListenSocket == INVALID_SOCKET) {
		sprintf_s(sockinfo->errmsg, "socket failed with error: %d\n", WSAGetLastError());
		WSACleanup();
		return false;
	}
	// The sockaddr_in structure specifies the address family,
	// IP address, and port for the socket that is being bound.
	sockaddr_in service;
	service.sin_family = AF_INET;
	service.sin_addr.s_addr = inet_addr(_host);
	service.sin_port = htons(_port);

	// Setup the TCP listening socket
	if (bind(sockinfo->ListenSocket, (SOCKADDR *)& service, sizeof(service)) == SOCKET_ERROR) {

		sprintf_s(sockinfo->errmsg, "bind failed with error: %d\n", WSAGetLastError());
		closesocket(sockinfo->ListenSocket);
		WSACleanup();
		return false;
	}
	DWORD threadfuncID;
	HANDLE 	myHandle = CreateThread(0, 0, threadfunc, _sockinfo, 0, &threadfuncID);

	return true;
}

void ThreadedSocketServer::stop(void)
{
	socket_info_t * sockinfo = (socket_info_t *)_sockinfo;

	int iResult = shutdown(sockinfo->ClientSocket, SD_SEND);
	if (iResult == SOCKET_ERROR) {
		sprintf_s(sockinfo->errmsg, "shutdown failed with error: %d\n", WSAGetLastError());
		closesocket(sockinfo->ClientSocket);
		WSACleanup();
		return;
	}

	// cleanup
	closesocket(sockinfo->ClientSocket);
	sockinfo->ListenSocket = INVALID_SOCKET;
	sockinfo->ClientSocket = INVALID_SOCKET;
	WSACleanup();
}

bool ThreadedSocketServer::connected(void)
{
	socket_info_t * sockinfo = (socket_info_t *)_sockinfo;

	return sockinfo->ClientSocket != INVALID_SOCKET;
}

int ThreadedSocketServer::sendBuffer(char * buf, int len)
{
	socket_info_t * sockinfo = (socket_info_t *)_sockinfo;

	// Echo the buffer back to the sender
	int iSendResult = send(sockinfo->ClientSocket, buf, len, 0);
	if (iSendResult == SOCKET_ERROR) {
		closesocket(sockinfo->ClientSocket);
		WSACleanup();
		return 0;
	}
	return iSendResult;
}

int ThreadedSocketServer::receiveBuffer(char * buf, int len)
{
	socket_info_t * sockinfo = (socket_info_t *)_sockinfo;

	return recv(sockinfo->ClientSocket, buf, len, 0);

}

