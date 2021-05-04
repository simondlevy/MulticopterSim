-- // http://book.realworldhaskell.org/read/sockets-and-syslog.html
import Data.Bits
import Network.Socket
import Network.Socket.ByteString
import Network.BSD
import Data.List
import Data.ByteString.Internal

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Char (ord)

import Data.ByteString.Internal as BS
import qualified Data.Vector.Storable as V

type HandlerFunc = SockAddr -> Data.ByteString.Internal.ByteString -> IO ()

serveSocket :: String              -- ^ Port number or name
         -> HandlerFunc         -- ^ Function to handle incoming messages
         -> IO ()
serveSocket port handlerfunc = withSocketsDo $
    do -- Look up the port.  Either raises an exception or returns
       -- a nonempty list.  
       addrinfos <- getAddrInfo 
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                    Nothing (Just port)
       let serveraddr = head addrinfos

       -- Create a socket
       sock <- socket (addrFamily serveraddr) Datagram defaultProtocol

       -- Bind it to the address we're listening to
       -- bindSocket sock (addrAddress serveraddr)
       bind sock (addrAddress serveraddr)

       -- Loop forever processing incoming data.  Ctrl-C to abort.
       procMessages sock
    where procMessages sock =
              do -- Receive one UDP packet, maximum length 1024 bytes,
                 -- and save its content into msg and its source
                 -- IP and port into addr
                 -- (msg, _, addr) <- recvFrom sock 1024
                 (msg, addr) <- Network.Socket.ByteString.recvFrom sock 104
                 -- Handle it
                 handlerfunc addr msg
                 -- And process more messages
                 procMessages sock

-- A simple handler that prints incoming packets
plainHandler :: HandlerFunc
plainHandler addr msg = 
    do
        print (convert msg)

convert :: ByteString -> Double
convert bs = 0

packStr :: String -> B.ByteString
packStr = B.pack . map (fromIntegral . ord)

bytesToDoubles :: BS.ByteString -> V.Vector Double
bytesToDoubles = V.unsafeCast . aux . BS.toForeignPtr
    where aux (fp,offset,len) = V.unsafeFromForeignPtr fp offset len

main :: IO ()
-- main = serveSocket "5001" plainHandler
main = print (bytesToDoubles (packStr "\NUL\NUL\NUL\NUL\NUL\NUL\240?"))
