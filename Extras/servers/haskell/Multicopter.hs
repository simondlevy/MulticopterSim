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

runMulticopter :: ControlFunc -> IO ()
runMulticopter controlFunc = withSocketsDo $

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

       -------------------------------------------------------

       bind telemetryServerSocket telemetrySockAddr

       putStrLn "Hit the Start button ..."

       processMessages telemetryServerSocket motorClientSocket motorSockAddr

    where processMessages telemetryServerSocket motorClientSocket motorClientSockAddr =
              do 

                  (msgIn, _) <- Network.Socket.ByteString.recvFrom telemetryServerSocket 104

                  let msgVals = bytesToDoubles msgIn

                  let t = Time (head msgVals)
                  let x = State (tail msgVals)
                  let motors = (controlFunc t x)
                  _ <- Network.Socket.ByteString.sendTo
                        motorClientSocket
                        (doublesToBytes [(m1 motors), (m2 motors), (m3 motors), (m4 motors)])
                        motorClientSockAddr

                  processMessages telemetryServerSocket motorClientSocket motorClientSockAddr
                      

-- https://stackoverflow.com/questions/20912582/haskell-bytestring-to-float-array

doublesToBytes :: [Double] -> ByteString
doublesToBytes = runPut . mapM_ putFloat64le

bytesToDoubles :: ByteString -> [Double]
bytesToDoubles bs = (fromRight ((runGet $ many getFloat64le) bs))
