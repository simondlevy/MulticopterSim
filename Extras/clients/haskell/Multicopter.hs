{--
  Socket-based multicopter control

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

module Multicopter (runMulticopter) where

import Control.Applicative
import Data.Serialize
import Network.Socket
import Network.Socket.ByteString
import Data.ByteString.Internal

runMulticopter :: motorfun -> IO ()
runMulticopter motorfun = withSocketsDo $

   -- Adapted from http://book.realworldhaskell.org/read/sockets-and-syslog.html

    do 

       -- XXX should replace this repeated code with a datatype

       telemetryServerAddrInfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "5001")
       let telemetryServerAddr = head telemetryServerAddrInfo
       telemetryServerSocket <- socket (addrFamily telemetryServerAddr) Datagram defaultProtocol
       let telemetrySockAddr = addrAddress telemetryServerAddr

       motorClientAddrInfo <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just "5000")
       let motorClientAddr = head motorClientAddrInfo
       motorClientSocket <- socket (addrFamily motorClientAddr) Datagram defaultProtocol
       let motorSockAddr = addrAddress motorClientAddr

       -- XXX --------------------------------------------------

       bind telemetryServerSocket telemetrySockAddr

       processMessages telemetryServerSocket motorClientSocket motorSockAddr

    where processMessages telemetryServerSocket motorClientSocket motorClientSockAddr =
              do 
                  (msgIn, _) <- Network.Socket.ByteString.recvFrom telemetryServerSocket 104
                  _ <- Network.Socket.ByteString.sendTo
                        motorClientSocket
                        (doublesToBytes (getMotors (bytesToDoubles msgIn)))
                        motorClientSockAddr
                  processMessages telemetryServerSocket motorClientSocket motorClientSockAddr
                      
-- https://stackoverflow.com/questions/20912582/haskell-bytestring-to-float-array

getMotors :: Either String [Double] -> [Double]
getMotors x = [0.6,0.6,0.6,0.6]

doublesToBytes :: [Double] -> ByteString
doublesToBytes = runPut . mapM_ putFloat64le

bytesToDoubles :: ByteString -> Either String [Double]
bytesToDoubles = runGet $ many getFloat64le
