'''
  MultiCopter client class

  Copyright(C) 2023 Simon D.Levy

  MIT License
'''

import socket


class MulticopterClient(object):

    def __init__(
            self,
            host='127.0.0.1',
            telemetry_port=5000):

        self.host = host
        self.telemetry_port = telemetry_port

    def start(self):

        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:

            done = False

            try:

                print('************************ Connecting')

                sock.connect((self.host, self.telemetry_port))

                while not done:

                    try:

                        telemetry_bytes = sock.recv(8*17)

                    except Exception as e:
                        done = True
                        print(str(e))
                        break

            except ConnectionRefusedError:

                print('Connection error; did you start the server first?')

