/*
* HackflightSimSocketClientWindows.h : Windwos Support for socket client
*
* Copyright (C) 2018 Simon D. Levy
*
* MIT License
*/

#pragma once
/**
 * 
 */
class HackflightSimSocketClient {

    public:

        HackflightSimSocketClient();

        ~HackflightSimSocketClient();

		bool isConnected();

		void attemptConnection();

	private:

	 const char * HOST = "localhost";
	 const char * PORT = "20000";
	 static const int    BUFLEN = 512;

	 void * _result;

	 void * _ConnectSocket;

	 bool _connected;

	 char _recvbuf[BUFLEN];
};
