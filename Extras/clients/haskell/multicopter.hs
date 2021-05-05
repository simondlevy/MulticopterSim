{--
  Socket-based multicopter control

  Copyright(C) 2021 Simon D.Levy

  MIT License
--}

import Control.Applicative
import Data.Serialize
import Network.Socket
import Network.Socket.ByteString
import Data.ByteString.Internal

runMulticopter :: IO ()
runMulticopter = withSocketsDo $

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
                  print (bytesToDoubles msgIn)
                  let msgOut = doublesToBytes [0.6,0.6,0.6,0.6]
                  _ <- Network.Socket.ByteString.sendTo motorClientSocket msgOut motorClientSockAddr
                  processMessages telemetryServerSocket motorClientSocket motorClientSockAddr
                      
-- https://stackoverflow.com/questions/20912582/haskell-bytestring-to-float-array

doublesToBytes :: [Double] -> ByteString
doublesToBytes = runPut . mapM_ putFloat64le

bytesToDoubles :: ByteString -> Either String [Double]
bytesToDoubles = runGet $ many getFloat64le

--------------------------------------------------------------------------

main :: IO ()
main = runMulticopter
