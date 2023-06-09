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

        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

        s.connect((self.host, self.telemetry_port))
