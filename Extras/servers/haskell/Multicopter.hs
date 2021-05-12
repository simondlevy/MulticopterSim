{--
  Socket-based multicopter control

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Multicopter (runMulticopter) where

import Control.Applicative
import Network.Socket
import Network.Socket.ByteString -- from network
import Data.ByteString.Internal
import Data.Either.Utils -- from MissingH
import Data.Serialize -- from cereal

import Types

-- Adapted from http://book.realworldhaskell.org/read/sockets-and-syslog.html

makeSocket :: String -> IO (Socket, AddrInfo)
makeSocket port =
    do 
       addrInfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
       let addr = head addrInfo
       sock <- socket (addrFamily addr) Datagram defaultProtocol
       return (sock, addr)


runMulticopter :: PidController -> Mixer -> IO ()
runMulticopter controller mixer = withSocketsDo $

    do 

       (telemetryServerSocket, telemetryServerAddr) <- makeSocket "5001"
       (motorClientSocket, motorClientAddr) <- makeSocket "5000"

       bind telemetryServerSocket (addrAddress telemetryServerAddr)

       putStrLn "Hit the Play button ..."

       processMessages telemetryServerSocket motorClientSocket (addrAddress motorClientAddr) (PidControllerState 0)

    where processMessages telemetryServerSocket motorClientSocket motorClientSockAddr controllerState =
              do 

                  (msgIn, _) <- Network.Socket.ByteString.recvFrom telemetryServerSocket 104

                  let v = bytesToDoubles msgIn

                  let t = Time (head v)
                  let vs = makeState (tail v)

                  let (demands, newControllerState) = controller t vs  demands controllerState
                  let motors = mixer demands
                  _ <- Network.Socket.ByteString.sendTo
                        motorClientSocket
                        (doublesToBytes [(m1 motors), (m2 motors), (m3 motors), (m4 motors)])
                        motorClientSockAddr

                  processMessages telemetryServerSocket motorClientSocket motorClientSockAddr newControllerState
                      

-- https://stackoverflow.com/questions/20912582/haskell-bytestring-to-float-array

doublesToBytes :: [Double] -> ByteString
doublesToBytes = runPut . mapM_ putFloat64le

bytesToDoubles :: ByteString -> [Double]
bytesToDoubles bs = (fromRight ((runGet $ many getFloat64le) bs))
